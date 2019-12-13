package net.verdagon.vale.vivem

import net.verdagon.vale.hammer._
import net.verdagon.vale.scout.{MemberRefCount, RefCountCategory, RegisterRefCount, VariableRefCount}
import net.verdagon.vale.templar.types.Ownership
import net.verdagon.vale.{vassert, vfail}

// RR = Runtime Result. Don't use these to determine behavior, just use
// these to check that things are as we expect.
case class RRReference(hamut: Reference3[Referend3])
case class RRReferend(hamut: Referend3)

class Allocation(
    val reference: ReferenceV, // note that this cannot change
    val referend: ReferendV // note that this cannot change
) {
  private var referrers = Set[IObjectReferrer]()

  def id = reference.allocId

  def incrementRefCount(referrer: IObjectReferrer) = {
    if (referrers.contains(referrer)) {
      vfail("nooo")
    }
    referrers = referrers + referrer
  }

  def decrementRefCount(referrer: IObjectReferrer) = {
    if (!referrers.contains(referrer)) {
      vfail("nooooo\n" + referrer + "\nnot in:\n" + referrers)
    }
    referrers = referrers - referrer
  }

  private def getCategory(referrer: IObjectReferrer) = {
    referrer match {
      case VariableToObjectReferrer(_) => VariableRefCount
      case MemberToObjectReferrer(_) => MemberRefCount
      case RegisterToObjectReferrer(_) => RegisterRefCount
      case ArgumentToObjectReferrer(_) => RegisterRefCount
    }
  }

  def getRefCount(category: RefCountCategory) = {
    referrers.map(getCategory).count(_ == category)
  }

  def ensureRefCount(category: RefCountCategory, expectedNum: Int) = {
    val matchingReferrers =
      referrers
        .map(referrer => (getCategory(referrer), referrer))
        .filter(_._1 == category)
        .map(_._2)
    if (matchingReferrers.size != expectedNum) {
      vfail(
        "Expected " + expectedNum + " " + category + " but was " + matchingReferrers.size + ":\n" +
        matchingReferrers.mkString("\n"))
    }
  }

  def ensureTotalRefCount(expectedNum: Int) = {
    if (referrers.size != expectedNum) {
      vfail(
        "o" + reference.allocId.num + " expected " + expectedNum + " but was " + referrers.size + ":\n" +
            referrers.mkString("\n") + "\nReferend:\n" + referend)
    }
  }

  def printRefs() = {
    if (getTotalRefCount() > 0) {
      println("o" + reference.allocId.num + ": " + referrers.mkString(" "))
    }
  }

  def getTotalRefCount() = {
    referrers.size
  }

  override def finalize(): Unit = {
    vassert(referrers.isEmpty)
  }

  def unapply(arg: Allocation): Option[ReferendV] = Some(referend)
}

object Allocation {
  def unapply(arg: Allocation): Option[ReferendV] = {
    Some(arg.referend)
  }
}

sealed trait ReferendV {
  def tyype: RRReferend
}
sealed trait PrimitiveReferendV extends ReferendV
case class IntV(value: Int) extends PrimitiveReferendV {
  override def tyype = RRReferend(Int3())
}
case class VoidV() extends PrimitiveReferendV {
  override def tyype = RRReferend(Void3())
}
case class BoolV(value: Boolean) extends PrimitiveReferendV {
  override def tyype = RRReferend(Bool3())
}
case class FloatV(value: Float) extends PrimitiveReferendV {
  override def tyype = RRReferend(Float3())
}
case class StrV(value: String) extends PrimitiveReferendV {
  override def tyype = RRReferend(Str3())
}

case class FunctionReferendV(function: Function3) extends ReferendV {
  override def tyype = RRReferend(function.prototype.functionType)
}

case class StructInstanceV(
    struct3: StructDefinition3,
    private var members: Vector[ReferenceV]
) extends ReferendV {
  override def tyype = RRReferend(struct3.getRef)

  def getReferenceMember(index: Int) = {
    (struct3.members(index).tyype, members(index)) match {
      case (_, ref) => ref
    }
  }

  def setReferenceMember(index: Int, reference: ReferenceV) = {
    members = members.updated(index, reference)
  }
}

case class ArrayInstanceV(
    type3: Reference3[Referend3],
    elementType3: Reference3[Referend3],
    private val size: Int,
    private var elements: Vector[ReferenceV]
) extends ReferendV {
  override def tyype = RRReferend(type3.kind)

  def getElement(index: Int): ReferenceV = {
    // Make sure we're initialized
    vassert(elements.size == size)
    if (index < 0 || index >= size) {
      throw PanicException();
    }
    elements(index)
  }

  def setElement(index: Int, ref: ReferenceV) = {
    // Make sure we're initialized
    vassert(elements.size == size)
    if (index < 0 || index >= size) {
      throw PanicException();
    }
    elements = elements.updated(index, ref)
  }

  def initializeElement(index: Int, ref: ReferenceV) = {
    // Make sure we're not yet initialized
    vassert(elements.size < size)
    // Make sure we're initializing the *next* empty slot
    vassert(index == elements.size)
    elements = elements :+ ref
  }

  def deinitializeElement(index: Int) = {
    // Make sure we're initializing the *next* empty slot
    if (index != elements.size - 1) {
      vfail("wot")
    }
    val ref = elements(index)
    elements = elements.slice(0, elements.size - 1)
    ref
  }

  def getSize() = {
    // Make sure we're initialized
    vassert(elements.size == size)
    size
  }
}

case class AllocationId(tyype: RRReferend, num: Int)

case class ReferenceV(
  // actualType and seenAsType will be different in the case of interface reference.
  // Otherwise they'll be the same.

  // What is the actual type of what we're pointing to (as opposed to an interface).
  // If we have a Car reference to a Civic, then this will be Civic.
  actualKind: RRReferend,
  // What do we see the type as. If we have a Car reference to a Civic, then this will be Car.
  seenAsKind: RRReferend,

  ownership: Ownership,

  num: Int
) {
  def allocId = AllocationId(RRReferend(actualKind.hamut), num)
  def actualCoord: RRReference = RRReference(Reference3(ownership, actualKind.hamut))
  def seenAsCoord: RRReference = RRReference(Reference3(ownership, seenAsKind.hamut))
}

sealed trait IObjectReferrer
case class VariableToObjectReferrer(varAddr: VariableAddressV) extends IObjectReferrer
case class MemberToObjectReferrer(memberAddr: MemberAddressV) extends IObjectReferrer
case class ElementToObjectReferrer(elementAddr: ElementAddressV) extends IObjectReferrer
case class RegisterToObjectReferrer(registerId: RegisterId) extends IObjectReferrer
// This is us holding onto something during a while loop or array generator call, so the called functions dont eat them and deallocate them
case class RegisterHoldToObjectReferrer(registerId: RegisterId) extends IObjectReferrer
case class ResultToObjectReferrer(callId: CallId) extends IObjectReferrer
case class ArgumentToObjectReferrer(argumentId: ArgumentId) extends IObjectReferrer

case class VariableAddressV(callId: CallId, local: Local) {
  override def toString: String = "&v:" + callId + "#v" + local.id
}
case class MemberAddressV(structId: AllocationId, fieldIndex: Int) {
  override def toString: String = "&o:" + structId.num + "." + fieldIndex
}
case class ElementAddressV(arrayId: AllocationId, elementIndex: Int) {
  override def toString: String = "&o:" + arrayId.num + "." + elementIndex
}

// Used in tracking reference counts/maps.
case class CallId(blockDepth: Int, function: Function3) {
  override def toString: String = "ƒ" + blockDepth + "/" + function.prototype.fullName.parts.head.humanName
}
case class RegisterId(blockId: BlockId, line: String)
case class ArgumentId(callId: CallId, index: Int)
case class VariableV(
    id: VariableAddressV,
    var reference: Option[ReferenceV],
    expectedType: Reference3[Referend3]) {
  vassert(reference != None)
}

case class BlockId(callId: CallId, blockHeight: Int)

sealed trait RegisterV {
  def expectReferenceRegister() = {
    this match {
      case rr @ ReferenceRegisterV(reference) => {
        rr
      }
    }
  }
}
case class ReferenceRegisterV(reference: ReferenceV) extends RegisterV


case class VivemPanic(message: String) extends Exception