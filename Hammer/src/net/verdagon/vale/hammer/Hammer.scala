package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.templar.env.FullName2
import net.verdagon.vale.{vassert, vfail}

case class FunctionRefH(prototype: PrototypeH) {
  //  def functionType = prototype.functionType
  def fullName = prototype.fullName
}

case class LocalsBox(var inner: Locals) {
  def snapshot = inner

  def templarLocals: Map[FullName2, VariableIdH] = inner.templarLocals
  def unstackifiedVars: Set[VariableIdH] = inner.unstackifiedVars
  def locals: Map[VariableIdH, Local] = inner.locals

  def get(id: FullName2) = inner.get(id)
  def get(id: VariableIdH) = inner.get(id)

  def markUnstackified(varId2: FullName2): Unit = {
    inner = inner.markUnstackified(varId2)
  }

  def markUnstackified(varIdH: VariableIdH): Unit = {
    inner = inner.markUnstackified(varIdH)
  }

  def addHammerLocal(
    height: StackHeight,
    tyype: ReferenceH[ReferendH]):
  Local = {
    val (newInner, local) = inner.addHammerLocal(height, tyype)
    inner = newInner
    local
  }

  def addTemplarLocal(
    varId2: FullName2,
    height: StackHeight,
    tyype: ReferenceH[ReferendH]):
  Local = {
    val (newInner, local) = inner.addTemplarLocal(varId2, height, tyype)
    inner = newInner
    local
  }

}

// This represents the locals for the entire function.
// Note, some locals will have the same index, that just means they're in
// different blocks.
case class Locals(
    // This doesn't have all the locals that are in the locals list, this just
    // has any locals added by templar.
    templarLocals: Map[FullName2, VariableIdH],

    unstackifiedVars: Set[VariableIdH],

    // This has all the locals for the function, a superset of templarLocals.
    locals: Map[VariableIdH, Local]) {

  def addTemplarLocal(
    varId2: FullName2,
    height: StackHeight,
    tyype: ReferenceH[ReferendH]):
  (Locals, Local) = {
    if (templarLocals.contains(varId2)) {
      vfail("wot")
    }
    val newLocalIdNumber = locals.size
    val newLocalId = VariableIdH(newLocalIdNumber, Some(varId2.variableName))
    val newLocal = Local(newLocalId, height, tyype)
    val newLocals =
      Locals(
        templarLocals + (varId2 -> newLocalId),
        unstackifiedVars,
        locals + (newLocalId -> newLocal))
    (newLocals, newLocal)
  }

  def addHammerLocal(
    height: StackHeight,
    tyype: ReferenceH[ReferendH]):
  (Locals, Local) = {
    val newLocalIdNumber = locals.size
    val newLocalId = VariableIdH(newLocalIdNumber, None)
    val newLocal = Local(newLocalId, height, tyype)
    val newLocals =
      Locals(
        templarLocals,
        unstackifiedVars,
        locals + (newLocalId -> newLocal))
    (newLocals, newLocal)
  }

  def markUnstackified(varId2: FullName2): Locals = {
    markUnstackified(templarLocals(varId2))
  }

  def markUnstackified(varIdH: VariableIdH): Locals = {
    // Make sure it existed and wasnt already unstackified
    vassert(locals.contains(varIdH))
    if (unstackifiedVars.contains(varIdH)) {
      vfail("nooo")
    }
    Locals(templarLocals, unstackifiedVars + varIdH, locals)
  }

  def get(varId: FullName2): Option[Local] = {
    templarLocals.get(varId) match {
      case None => None
      case Some(index) => Some(locals(index))
    }
  }

  def get(varId: VariableIdH): Option[Local] = {
    locals.get(varId)
  }
}

object Hammer {
  def translate(hinputs: Hinputs): ProgramH = {
    val hamuts = HamutsBox(Hamuts(Map(), Map(), List(), Map(), Map(), Map(), Map()))
    val emptyPackStructRefH = StructHammer.translateStructRef(hinputs, hamuts, hinputs.program2.emptyPackStructRef)
    StructHammer.translateInterfaces(hinputs, hamuts);
    StructHammer.translateStructs(hinputs, hamuts)
    val userFunctions = hinputs.program2.functions.filter(_.header.isUserFunction).toList
    val nonUserFunctions = hinputs.program2.functions.filter(!_.header.isUserFunction).toList
    FunctionHammer.translateFunctions(hinputs, hamuts, userFunctions)
    FunctionHammer.translateFunctions(hinputs, hamuts, nonUserFunctions)

    ProgramH(
      hamuts.interfaceDefs.values.toList,
      hamuts.structDefsByRef2.values.toList,
      emptyPackStructRefH,
      List() /* externs */,
      hamuts.functionDefs.values.toList)
  }
}

case class NodesBox(var inner: Vector[NodeH]) {
  def addNode(node: NodeH): NodeH = {
    inner = inner :+ node
    node
  }

  def nextId(): String = {
    "" + inner.size
  }
}