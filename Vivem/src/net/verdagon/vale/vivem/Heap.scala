package net.verdagon.vale.vivem

import java.io.PrintStream

import net.verdagon.vale.hammer.NameHammer
import net.verdagon.vale.metal._

//import net.verdagon.vale.hammer._
//import net.verdagon.vale.scout.RefCountCategory
//import net.verdagon.vale.templar.types.{Ownership, Raw, Share}
import net.verdagon.vale.{vassert, vcurious, vfail, vimpl}
import net.verdagon.von._

import scala.collection.mutable
// A wrapper that represents us holding a reference count +1 to this reference.
case class ReturnV(blockId: BlockId, reference: ReferenceV)

class AdapterForExterns(
    val programH: ProgramH,
    private val heap: Heap,
    blockId: BlockId,
    val stdin: (() => String),
    val stdout: (String => Unit),
    val dropReferenceIfNonOwning: (ReferenceV => Unit)
) {
  def dereference(reference: ReferenceV) = {
    heap.dereference(reference)
  }

  def addAllocationForReturn(ownership: Ownership, referend: ReferendV): ReturnV = {
    val ref = heap.add(ownership, referend)
    heap.incrementReferenceRefCount(ResultToObjectReferrer(blockId.callId), ref) // incrementing because putting it in a return
    ReturnV(blockId, ref)
  }
}

class AllocationMap(vivemDout: PrintStream) {
  private val objectsById = mutable.HashMap[AllocationId, Allocation]()

  private var nextId = 501;
  private def newId() = {
    val id = nextId;
    nextId = nextId + 1
    id
  }

  def isEmpty: Boolean = {
    objectsById.isEmpty
  }

  def size = {
    objectsById.size
  }

  def get(reference: ReferenceV) = {
    val allocation = objectsById(reference.allocId)
    vassert(allocation.referend.tyype.hamut == reference.actualKind.hamut)
    allocation
  }

  def get(allocId: AllocationId) = {
    val allocation = objectsById(allocId)
    vassert(allocation.referend.tyype == allocId.tyype)
    allocation
  }

  def remove(allocId: AllocationId) = {
    vassert(contains(allocId))
    objectsById.remove(allocId)
  }

  def contains(reference: ReferenceV): Boolean = {
    contains(reference.allocId)
  }

  def contains(allocId: AllocationId): Boolean = {
    objectsById.get(allocId) match {
      case None => false
      case Some(allocation) => {
        vassert(allocation.referend.tyype.hamut == allocId.tyype.hamut)
        true
      }
    }
  }

  def add(ownership: Ownership, referend: ReferendV) = {
    val reference =
      ReferenceV(
        // These two are the same because when we allocate something,
        // we see it for what it truly is.
        //                                          ~ Wisdom ~
        actualKind = referend.tyype,
        seenAsKind = referend.tyype,
        ownership,
        newId())
    val allocation = new Allocation(reference, referend)
    objectsById.put(reference.allocId, allocation)
    reference
  }

  def printAll(): Unit = {
    objectsById.foreach({
      case (id, allocation) => vivemDout.println(id + " (" + allocation.getTotalRefCount() + " refs) = " + allocation.referend)
    })
  }

  def checkForLeaks(): Unit = {
    if (objectsById.nonEmpty) {
      objectsById.values.map(_.reference.allocId.num).toArray.sorted.foreach(objId => print("o" + objId + " "))
      println()
      objectsById.values.toArray.sortWith(_.reference.allocId.num < _.reference.allocId.num).foreach(_.printRefs())
      vfail("Memory leaks! See above for ")
    }
  }
}

// Just keeps track of all active objects
class Heap(in_vivemDout: PrintStream) {
  val vivemDout = in_vivemDout

  private val objectsById = new AllocationMap(vivemDout)
  
  private val callIdStack = mutable.Stack[CallId]();
  private val callsById = mutable.HashMap[CallId, Call]()

  def addLocal(varAddr: VariableAddressV, reference: ReferenceV, expectedType: ReferenceH[ReferendH]) = {
    val call = getCurrentCall(varAddr.callId)
    call.addLocal(varAddr, reference, expectedType)
    incrementReferenceRefCount(VariableToObjectReferrer(varAddr), reference)
  }

  def getReference(varAddr: VariableAddressV, expectedType: ReferenceH[ReferendH]) = {
    callsById(varAddr.callId).getLocal(varAddr).reference.get
  }

  def removeLocal(varAddr: VariableAddressV, expectedType: ReferenceH[ReferendH]) = {
    val variable = getLocal(varAddr)
    val actualReference = variable.reference.get
    checkReference(expectedType, actualReference)
    decrementReferenceRefCount(VariableToObjectReferrer(varAddr), actualReference)
    variable.reference = None
  }

  def getReferenceFromLocal(varAddr: VariableAddressV, expectedType: ReferenceH[ReferendH]): ReferenceV = {
    val variable = getLocal(varAddr)
    if (variable.expectedType != expectedType) {
      vfail("blort")
    }
    if (variable.reference.isEmpty) {
      vfail("Can't get from variable, it's already empty!")
    }
    checkReference(expectedType, variable.reference.get)
    variable.reference.get
  }

  private def getLocal(varAddr: VariableAddressV): VariableV = {
    callsById(varAddr.callId).getLocal(varAddr)
  }

  def mutateVariable(varAddress: VariableAddressV, reference: ReferenceV, expectedType: ReferenceH[ReferendH]): ReferenceV = {
    val variable = callsById(varAddress.callId).getLocal(varAddress)
    checkReference(expectedType, reference)
    checkReference(variable.expectedType, reference)
    val oldReference = variable.reference.get
    decrementReferenceRefCount(VariableToObjectReferrer(varAddress), oldReference)

    incrementReferenceRefCount(VariableToObjectReferrer(varAddress), reference)
    callsById(varAddress.callId).mutateLocal(varAddress, reference, expectedType)
    oldReference
  }
  def mutateArray(elementAddress: ElementAddressV, reference: ReferenceV, expectedType: ReferenceH[ReferendH]): ReferenceV = {
    val ElementAddressV(arrayRef, elementIndex) = elementAddress
    objectsById.get(arrayRef).referend match {
      case ai @ ArrayInstanceV(_, _, _, _) => {
        val oldReference = ai.getElement(elementIndex)
        decrementReferenceRefCount(ElementToObjectReferrer(elementAddress), oldReference)

        ai.setElement(elementIndex, reference)
        incrementReferenceRefCount(ElementToObjectReferrer(elementAddress), reference)
        oldReference
      }
    }
  }
  def mutateStruct(memberAddress: MemberAddressV, reference: ReferenceV, expectedType: ReferenceH[ReferendH]):
  ReferenceV = {
    val MemberAddressV(objectId, fieldIndex) = memberAddress
    objectsById.get(objectId).referend match {
      case si @ StructInstanceV(structDefH, members) => {
        val oldMemberReference = members(fieldIndex)
        decrementReferenceRefCount(MemberToObjectReferrer(memberAddress), oldMemberReference)
//        maybeDeallocate(actualReference)
        vassert(structDefH.members(fieldIndex).tyype == expectedType)
        // We only do this to check that it's non-empty. curiosity assert, do we gotta do somethin special if somethin was moved out
        si.getReferenceMember(fieldIndex)
        si.setReferenceMember(fieldIndex, reference)
        incrementReferenceRefCount(MemberToObjectReferrer(memberAddress), reference)
        oldMemberReference
      }
    }
  }
//
//  def blacklistElement(elementAddress: ElementAddressV, expectedType: ReferenceH[ReferendH]): Unit = {
//    objectsById.get(elementAddress.arrayId).referend match {
//      case ai @ ArrayInstanceV(_, _, _) => {
//        val ref = ai.getElement(elementAddress.elementIndex)
//        checkReference(expectedType, ref)
//        decrementReferenceRefCount(
//          ElementToObjectReferrer(elementAddress),
//          ref)
//        ai.blacklistElement(elementAddress.elementIndex)
//      }
//    }
//  }

  def getReferenceFromStruct(address: MemberAddressV, expectedType: ReferenceH[ReferendH]): ReferenceV = {
    val MemberAddressV(objectId, fieldIndex) = address
    objectsById.get(objectId).referend match {
      case StructInstanceV(_, members) => {
        val actualReference = members(fieldIndex)
        checkReference(expectedType, actualReference)
        actualReference
      }
    }
  }
  def getReferenceFromArray(address: ElementAddressV, expectedType: ReferenceH[ReferendH]): ReferenceV = {
    val ElementAddressV(objectId, elementIndex) = address
    objectsById.get(objectId).referend match {
      case ai @ ArrayInstanceV(_, _, _, _) => {
        val ref = ai.getElement(elementIndex)
        checkReference(expectedType, ref)
        ref
      }
    }
  }

  def dereference(reference: ReferenceV): ReferendV = {
    vassert(objectsById.contains(reference))
    objectsById.get(reference).referend
  }

  def incrementReferenceHoldCount(registerId: RegisterId, reference: ReferenceV) = {
    incrementObjectRefCount(RegisterHoldToObjectReferrer(registerId), reference.allocId)
  }

  def decrementReferenceHoldCount(registerId: RegisterId, reference: ReferenceV) = {
    decrementObjectRefCount(RegisterHoldToObjectReferrer(registerId), reference.allocId)
  }

  // rename to incrementObjectRefCount
  def incrementReferenceRefCount(referrer: IObjectReferrer, reference: ReferenceV) = {
    incrementObjectRefCount(referrer, reference.allocId)
  }

  // rename to decrementObjectRefCount
  def decrementReferenceRefCount(referrer: IObjectReferrer, reference: ReferenceV) = {
    decrementObjectRefCount(referrer, reference.allocId)
  }

  def destructure(reference: ReferenceV): Vector[ReferenceV] = {
    val allocation = dereference(reference)
    allocation match {
      case StructInstanceV(structDefH, memberRefs) => {
        memberRefs.zipWithIndex.foreach({ case (memberRef, index) =>
          decrementReferenceRefCount(MemberToObjectReferrer(MemberAddressV(reference.allocId, index)), memberRef)
        })
        deallocate(reference)
        memberRefs
      }
    }
  }

  def deallocate(reference: ReferenceV) = {
    val allocation = objectsById.get(reference)
    vassert(allocation.getTotalRefCount() == 0)
    objectsById.remove(reference.allocId)
    vivemDout.print(" o" + reference.allocId.num + "dealloc")
  }

  private def incrementObjectRefCount(pointingFrom: IObjectReferrer, allocId: AllocationId) = {
    if (!objectsById.contains(allocId)) {
      vfail("Trying to increment dead object: " + allocId)
    }
    val obj = objectsById.get(allocId)
    obj.incrementRefCount(pointingFrom)
    val newRefCount = obj.getTotalRefCount()
    vivemDout.print(" o" + allocId.num + "rc" + (newRefCount - 1) + "->" + newRefCount)
  }

  private def decrementObjectRefCount(pointedFrom: IObjectReferrer, allocId: AllocationId): Int = {
    if (!objectsById.contains(allocId)) {
      vfail("Can't decrement object " + allocId + ", not in heap!")
    }
    val obj = objectsById.get(allocId)
    obj.decrementRefCount(pointedFrom)
    val newRefCount = obj.getTotalRefCount()
    vivemDout.print(" o" + allocId.num + "rc" + (newRefCount + 1) + "->" + newRefCount)
//    if (newRefCount == 0) {
//      deallocate(objectId)
//    }
    newRefCount
  }

  def getRefCount(reference: ReferenceV, category: RefCountCategory): Int = {
    vassert(objectsById.contains(reference))
    val allocation = objectsById.get(reference)
    allocation.getRefCount(category)
  }

  def getTotalRefCount(reference: ReferenceV): Int = {
    vassert(objectsById.contains(reference))
    val allocation = objectsById.get(reference)
    allocation.getTotalRefCount()
  }

  def ensureRefCount(reference: ReferenceV, category: RefCountCategory, expectedNum: Int) = {
    vassert(objectsById.contains(reference))
    val allocation = objectsById.get(reference)
    allocation.ensureRefCount(category, expectedNum)
  }

  def ensureTotalRefCount(reference: ReferenceV, expectedNum: Int) = {
    vassert(objectsById.contains(reference))
    val allocation = objectsById.get(reference)
    allocation.ensureTotalRefCount(expectedNum)
  }

  def add(ownership: Ownership, referend: ReferendV): ReferenceV = {
    objectsById.add(ownership, referend)
  }

  def alias(reference: ReferenceV, expectedType: ReferenceH[ReferendH], targetOwnership: Ownership): ReferenceV = {
    val ReferenceV(actualKind, oldSeenAsType, oldOwnership, objectId) = reference
    vassert((oldOwnership == Share) == (targetOwnership == Share))
    if (oldSeenAsType.hamut != expectedType.kind) {
      // not sure if the above .actualType is right

      vfail("wot")
    }
    ReferenceV(
      actualKind,
      RRReferend(expectedType.kind),
      targetOwnership,
      objectId)
  }

  def isEmpty: Boolean = {
    objectsById.isEmpty
  }

  def printAll() = {
    objectsById.printAll()
  }

  def countUnreachableAllocations(roots: Vector[ReferenceV]) = {
    val numReachables = findReachableAllocations(roots).size
    vassert(numReachables <= objectsById.size)
    objectsById.size - numReachables
  }

  def findReachableAllocations(
      inputReachables: Vector[ReferenceV]): Map[ReferenceV, Allocation] = {
    val destinationMap = mutable.Map[ReferenceV, Allocation]()
    inputReachables.foreach(inputReachable => {
      innerFindReachableAllocations(destinationMap, inputReachable)
    })
    destinationMap.toMap
  }

  private def innerFindReachableAllocations(
      destinationMap: mutable.Map[ReferenceV, Allocation],
      inputReachable: ReferenceV): Unit = {
    // Doublecheck that all the inputReachables are actually in this ..
    vassert(objectsById.contains(inputReachable))
    vassert(objectsById.get(inputReachable).referend.tyype.hamut == inputReachable.actualKind.hamut)

    val allocation = objectsById.get(inputReachable)
    if (destinationMap.contains(inputReachable)) {
      return
    }

    destinationMap.put(inputReachable, allocation)
    allocation.referend match {
      case IntV(_) =>
      case BoolV(_) =>
      case FloatV(_) =>
      case StructInstanceV(structDefH, members) => {
        members.zip(structDefH.members).foreach({
          case (reference, StructMemberH(_, _, referenceH)) => {
            innerFindReachableAllocations(destinationMap, reference)
          }
        })
      }
    }
  }

  def checkForLeaks(): Unit = {
    objectsById.checkForLeaks()
  }

  def getCurrentCall(expectedCallId: CallId) = {
    vassert(callIdStack.top == expectedCallId)
    callsById(expectedCallId)
  }

  def moveArgumentIntoRegister(registerId: RegisterId, argumentIndex: Int, expectedType: ReferenceH[ReferendH]) = {
    val reference = getCurrentCall(registerId.blockId.callId).takeArgument(argumentIndex)
    checkReference(expectedType, reference)
    setReferenceRegister(registerId, reference) // this increments it
    decrementReferenceRefCount(
      ArgumentToObjectReferrer(ArgumentId(registerId.blockId.callId, argumentIndex)),
      reference) // decrementing because taking it out of arg
    // Now, the register is the only one that has this reference.
  }

  def returnFromRegister(registerId: RegisterId, expectedType: ReferenceH[ReferendH]) = {
    val ref = takeReferenceFromRegister(registerId, expectedType)
    incrementReferenceRefCount(
      ResultToObjectReferrer(registerId.blockId.callId),
      ref) // incrementing because putting it into the return slot
    ReturnV(registerId.blockId, ref)
  }

  // For example, for the integer we pass into the array generator
  def allocateTransient(ownership: Ownership, referend: ReferendV) = {
    val ref = add(ownership, referend)
    vivemDout.print(" o" + ref.allocId.num + "=")
    printReferend(referend)
    ref
  }

  def aliasIntoRegister(registerId: RegisterId, reference: ReferenceV, expectedType: ReferenceH[ReferendH], targetOwnership: Ownership) = {
    val ref = alias(reference, expectedType, targetOwnership)
    setReferenceRegister(registerId, ref)
  }

  def printReferend(referend: ReferendV) = {
    referend match {
      case VoidV() => vivemDout.print("ø")
      case IntV(value) => vivemDout.print(value)
      case BoolV(value) => vivemDout.print(value)
      case StrV(value) => vivemDout.print(value)
      case FloatV(value) => vivemDout.print(value)
      case StructInstanceV(structH, members) => vivemDout.print(structH.fullName + "{" + members.map("o" + _.allocId.num).mkString(", ") + "}")
      case ArrayInstanceV(typeH, memberTypeH, size, elements) => vivemDout.print("array:" + size + ":" + memberTypeH + "{" + elements.map("o" + _.allocId.num).mkString(", ") + "}")
    }
  }

  def setReferenceRegister(registerId: RegisterId, reference: ReferenceV) = {
    val call = getCurrentCall(registerId.blockId.callId)
    incrementReferenceRefCount(RegisterToObjectReferrer(registerId), reference) // incrementing because putting it into a register
    call.setRegister(registerId, ReferenceRegisterV(reference))
    vivemDout.print(" r" + registerId.line + "<-o" + reference.allocId.num)
  }

  def setReferenceRegisterFromReturn(registerId: RegisterId, ret: ReturnV) = {
    incrementReferenceRefCount(RegisterToObjectReferrer(registerId), ret.reference)
    decrementReferenceRefCount(ResultToObjectReferrer(ret.blockId.callId), ret.reference)
    getCurrentCall(registerId.blockId.callId)
      .setRegister(registerId, ReferenceRegisterV(ret.reference))
    vivemDout.print(" r" + registerId.line + "<-o" + ret.reference.allocId.num)
  }

  def getReferenceFromReturn(ret: ReturnV) = {
    decrementReferenceRefCount(ResultToObjectReferrer(ret.blockId.callId), ret.reference)
    ret.reference
  }


  def deallocateFromReturn(ret: ReturnV) = {
    decrementReferenceRefCount(ResultToObjectReferrer(ret.blockId.callId), ret.reference)
    deallocate(ret.reference)
  }

  def initializeArrayElementFromReturn(
      arrayReference: ReferenceV,
      index: Int,
      ret: ReturnV) = {
    dereference(arrayReference) match {
      case a @ ArrayInstanceV(_, _, _, _) => {
        decrementReferenceRefCount(ResultToObjectReferrer(ret.blockId.callId), ret.reference)
        incrementReferenceRefCount(
          ElementToObjectReferrer(ElementAddressV(arrayReference.allocId, index)),
          ret.reference)
        a.initializeElement(index, ret.reference)
      }
    }
  }

  def newStruct(
      registerId: RegisterId,
      structDefH: StructDefinitionH,
      structRefH: ReferenceH[StructRefH],
      memberReferences: List[ReferenceV]):
  ReferenceV = {
    val instance = StructInstanceV(structDefH, memberReferences.toVector)
    val reference = add(structRefH.ownership, instance)

    memberReferences.zipWithIndex.foreach({ case (memberReference, index) =>
      incrementReferenceRefCount(
        MemberToObjectReferrer(MemberAddressV(reference.allocId, index)),
        memberReference)
    })

    vivemDout.print(" o" + reference.num + "=")
    printReferend(instance)
    reference
  }

  def deinitializeArrayElement(arrayReference: ReferenceV, index: Int) = {
    val arrayInstance @ ArrayInstanceV(_, _, _, _) = dereference(arrayReference)
    val elementReference = arrayInstance.deinitializeElement(index)
    decrementReferenceRefCount(
      ElementToObjectReferrer(ElementAddressV(arrayReference.allocId, index)),
      elementReference)
    elementReference
  }

  def initializeArrayElementFromRegister(
      arrayReference: ReferenceV,
      index: Int,
      elementReference: ReferenceV) = {
    val arrayInstance @ ArrayInstanceV(_, _, _, _) = dereference(arrayReference)
    incrementReferenceRefCount(
      ElementToObjectReferrer(ElementAddressV(arrayReference.allocId, index)),
      elementReference)
    arrayInstance.initializeElement(index, elementReference)
  }

  def discardReturn(ret: ReturnV) = {
    decrementReferenceRefCount(ResultToObjectReferrer(ret.blockId.callId), ret.reference)
//    maybeDeallocate(ret.reference.allocId)
  }

  def takeReferenceFromRegister(registerId: RegisterId, expectedType: ReferenceH[ReferendH]) = {
    val register = getCurrentCall(registerId.blockId.callId).takeRegister(registerId)
    val ref = checkReferenceRegister(expectedType, register).reference
    decrementReferenceRefCount(RegisterToObjectReferrer(registerId), ref)
    ref
  }

  def takeReferencesFromRegistersInReverse(blockId: BlockId, registerIds: List[RegisterAccessH[ReferendH]]): List[ReferenceV] = {
    registerIds
        .reverse
        .map({
          case RegisterAccessH(argRegisterId, expectedType) => {
            takeReferenceFromRegister(RegisterId(blockId, argRegisterId), expectedType)
          }
        })
        .reverse
  }

  def allocateIntoRegister(registerId: RegisterId, ownership: Ownership, referend: ReferendV): ReferenceV = {
    val ref = add(ownership, referend)
    vivemDout.print(" o" + ref.allocId.num + "=")
    printReferend(referend)
    setReferenceRegister(registerId, ref)
    ref
  }

  def addUninitializedArray(
      arrayRefType: ReferenceH[UnknownSizeArrayTH],
      size: Int):
  (ReferenceV, ArrayInstanceV) = {
    val instance = ArrayInstanceV(arrayRefType, arrayRefType.kind.rawArray.elementType, size, Vector())
    val reference = add(arrayRefType.ownership, instance)
    (reference, instance)
  }

  def addArray(
    arrayRefType: ReferenceH[KnownSizeArrayTH],
    memberRefs: List[ReferenceV]):
  (ReferenceV, ArrayInstanceV) = {
    val instance = ArrayInstanceV(arrayRefType, arrayRefType.kind.rawArray.elementType, memberRefs.size, memberRefs.toVector)
    val reference = add(arrayRefType.ownership, instance)
    memberRefs.zipWithIndex.foreach({ case (memberRef, index) =>
      incrementReferenceRefCount(ElementToObjectReferrer(ElementAddressV(reference.allocId, index)), memberRef)
    })
    (reference, instance)
  }


  def checkReference(expectedType: ReferenceH[ReferendH], actualReference: ReferenceV): Unit = {
    if (actualReference.seenAsCoord.hamut != expectedType) {
      vfail("Expected " + expectedType + " but was " + actualReference.seenAsCoord.hamut)
    }
    val actualReferend = dereference(actualReference)
    checkReferend(expectedType.kind, actualReferend)
  }


  def checkReferenceRegister(tyype: ReferenceH[ReferendH], register: RegisterV): ReferenceRegisterV = {
    val reg = register.expectReferenceRegister()
    checkReference(tyype, reg.reference)
    reg
  }

  def checkReferend(expectedType: ReferendH, actualReferend: ReferendV): Unit = {
    (actualReferend, expectedType) match {
      case (IntV(_), IntH()) =>
      case (BoolV(_), BoolH()) =>
      case (StrV(_), StrH()) =>
      case (FloatV(_), FloatH()) =>
      case (VoidV(), VoidH()) =>
      case (StructInstanceV(structDefH, _), structRefH @ StructRefH(_)) => {
        if (structDefH.getRef != structRefH) {
          vfail("Expected " + structRefH + " but was " + structDefH)
        }
      }
      case (ArrayInstanceV(typeH, actualElementTypeH, _, _), arrayH @ UnknownSizeArrayTH(_)) => {
        if (typeH.kind != arrayH) {
          vfail("Expected " + arrayH + " but was " + typeH)
        }
      }
      case (ArrayInstanceV(typeH, actualElementTypeH, _, _), arrayH @ KnownSizeArrayTH(_, _)) => {
        if (typeH.kind != arrayH) {
          vfail("Expected " + arrayH + " but was " + typeH)
        }
      }
      case (StructInstanceV(structDefH, _), irH @ InterfaceRefH(_)) => {
        val structImplementsInterface =
          structDefH.edges.exists(_.interface == irH)
        if (!structImplementsInterface) {
          vfail("Struct " + structDefH.getRef + " doesnt implement interface " + irH);
        }
      }
      case (a, b) => {
        vfail("Mismatch! " + a + " is not a " + b)
      }
    }
  }

  def checkStructId(expectedStructType: StructRefH, expectedStructPointerType: ReferenceH[ReferendH], register: RegisterV): AllocationId = {
    val reference = checkReferenceRegister(expectedStructPointerType, register).reference
    dereference(reference) match {
      case siv @ StructInstanceV(structDefH, _) => {
        vassert(structDefH.getRef == expectedStructType)
      }
      case _ => vfail("Expected a struct but was " + register)
    }
    reference.allocId
  }

  def checkStructReference(expectedStructType: StructRefH, expectedStructPointerType: ReferenceH[ReferendH], register: RegisterV): StructInstanceV = {
    val reference = checkReferenceRegister(expectedStructPointerType, register).reference
    dereference(reference) match {
      case siv @ StructInstanceV(structDefH, _) => {
        vassert(structDefH.getRef == expectedStructType)
        siv
      }
      case _ => vfail("Expected a struct but was " + register)
    }
  }

  def checkStructReference(expectedStructType: StructRefH, reference: ReferenceV): StructInstanceV = {
    dereference(reference) match {
      case siv @ StructInstanceV(structDefH, _) => {
        vassert(structDefH.getRef == expectedStructType)
        siv
      }
      case _ => vfail("Expected a struct but was " + reference)
    }
  }

  def pushNewStackFrame(functionH: FunctionH, args: Vector[ReferenceV]) = {
    vassert(callsById.size == callIdStack.size)
    val callId =
      CallId(
        if (callIdStack.nonEmpty) callIdStack.top.blockDepth + 1 else 0,
        functionH)
    val call = new Call(callId, args)
    callsById.put(callId, call)
    callIdStack.push(callId)
    vassert(callsById.size == callIdStack.size)
    callId
  }

  def popStackFrame(expectedCallId: CallId): Unit = {
    vassert(callsById.size == callIdStack.size)
    vassert(callIdStack.top == expectedCallId)
    val call = callsById(expectedCallId)
    call.prepareToDie()
    callIdStack.pop()
    callsById.remove(expectedCallId)
    vassert(callsById.size == callIdStack.size)
  }

  def pushNewBlock(callId: CallId): BlockId = {
    vassert(callsById.size == callIdStack.size)
    getCurrentCall(callId).pushNewBlock()
  }

  def popBlock(blockId: BlockId): Unit = {
    vassert(callsById.size == callIdStack.size)
    getCurrentCall(blockId.callId).popBlock(blockId)
    vassert(callsById.size == callIdStack.size)
  }

  def toVon(ref: ReferenceV): IVonData = {
    dereference(ref) match {
      case IntV(value) => VonInt(value)
      case FloatV(value) => VonFloat(value)
      case BoolV(value) => VonBool(value)
      case StrV(value) => VonStr(value)
      case ArrayInstanceV(typeH, elementTypeH, size, elements) => {
        VonArray(None, elements.map(toVon))
      }
      case StructInstanceV(structH, members) => {
        vassert(members.size == structH.members.size)
        VonObject(
          structH.fullName.toString,
          None,
          structH.members.zip(members).zipWithIndex.map({ case ((memberH, memberV), index) =>
            VonMember(None, Some(vimpl(memberH.name.von.toString)), toVon(memberV))
          }).toVector)
      }
    }
  }

  def getVarAddress(callId: CallId, local: Local) = {
    VariableAddressV(callId, local)
  }
}
