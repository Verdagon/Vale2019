package net.verdagon.vale.vivem

import net.verdagon.vale.hammer.{Reference3, Referend3}
import net.verdagon.vale.{vassert, vfail}

import scala.collection.mutable

class Call(callId: CallId, in_args: Vector[ReferenceV]) {
  private val args = mutable.HashMap[Int, Option[ReferenceV]]() ++ in_args.indices.zip(in_args.map(arg => Some(arg))).toMap

  private val locals = mutable.HashMap[VariableAddressV, VariableV]()
  private val localAddrStack = mutable.Stack[VariableAddressV]()

  private val blockIdStack = mutable.Stack[BlockId]();

  private val registersById = mutable.HashMap[RegisterId, Option[RegisterV]]()
  private val registerIdStack = mutable.Stack[RegisterId]()

  def addLocal(varAddr: VariableAddressV, reference: ReferenceV, tyype: Reference3[Referend3]): Unit = {
    vassert(varAddr.callId == callId)
    vassert(varAddr.local.height.localsHeight == localAddrStack.size)
    vassert(!locals.contains(varAddr))
    localAddrStack.push(varAddr)
    locals.put(varAddr, VariableV(varAddr, Some(reference), tyype))
  }

  def getLocal(addr: VariableAddressV) = {
    locals(addr)
  }

  def mutateLocal(varAddr: VariableAddressV, reference: ReferenceV, expectedType: Reference3[Referend3]): Unit = {
    locals(varAddr).reference = Some(reference)
  }

  def takeArgument(index: Int): ReferenceV = {
    args(index) match {
      case Some(ref) => {
        args.put(index, None)
        ref
      }
      case None => {
        vfail("Already took from argument " + index)
      }
    }
  }

  def setRegister(registerId: RegisterId, register: RegisterV) {
    vassert(registerId.blockId == blockIdStack.top)
    vassert(!registersById.contains(registerId))
    registersById.put(registerId, Some(register))
    registerIdStack.push(registerId)
  }

  def takeRegister(registerId: RegisterId) = {
    vassert(registerId.blockId == blockIdStack.top)
    if (!registersById.contains(registerId)) {
      vfail("wot")
    }
    if (registerIdStack.isEmpty) {
      vfail("wot")
    }
    if (registerIdStack.top != registerId) {
      vfail("wot")
    }
    registersById(registerId) match {
      case None => {
        vfail("Already took from register " + registerId)
      }
      case Some(reg) => {
        registerIdStack.pop()
        registersById.put(registerId, None)
        reg
      }
    }
  }

  def pushNewBlock() = {
    val newBlockId =
      if (blockIdStack.isEmpty) {
        BlockId(callId, callId.blockDepth)
      } else {
        BlockId(callId, blockIdStack.top.blockHeight + 1)
      }
    blockIdStack.push(newBlockId)
    newBlockId
  }

  def popBlock(blockId: BlockId) = {
    if (registerIdStack.nonEmpty) {
      vassert(registerIdStack.top.blockId != blockId)
    }
    // Make sure all registers were taken
    val thisBlockRegisters = registersById.filter(_._1.blockId == blockId)
    thisBlockRegisters.foreach({ case (registerId, register) =>
      vassert(register == None)
      registersById.remove(registerId)
    })
    vassert(!registersById.exists(_._1.blockId == blockId))

    // Make sure all locals were unletted
    val thisBlockLocals = locals.filter({ case (varAddr, variable) =>
      varAddr.local.height.blockHeight == blockId.blockHeight - callId.blockDepth
    })
    thisBlockLocals.foreach({ case (varAddr, variable) =>
      vassert(variable.reference == None)
      locals.remove(varAddr)
    })
    vassert(!locals.exists(_._1.local.height.blockHeight == blockId.blockHeight))
    while (localAddrStack.nonEmpty && localAddrStack.top.local.height.blockHeight == blockId.blockHeight - callId.blockDepth) {
      localAddrStack.pop()
    }

    vassert(localAddrStack.size == locals.size)
    vassert(blockIdStack.top == blockId)
    blockIdStack.pop()
  }

  def prepareToDie() = {
    val undeadArgs =
      args.collect({
        case (index, Some(value)) => (index, value)
      })
    if (undeadArgs.nonEmpty) {
      vfail("Undead arguments:\n" + undeadArgs.mkString("\n"))
    }

    val undeadRegisters =
      registersById.collect({
        case (registerId, Some(register)) => (registerId, register)
      })
    if (undeadRegisters.nonEmpty) {
      vfail("Undead registers:\n" + undeadRegisters.mkString("\n"))
    }
  }
}
