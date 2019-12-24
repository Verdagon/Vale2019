package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar.env.VariableId2
import net.verdagon.vale.{vassert, vfail}

// This represents the locals for the entire function.
// Note, some locals will have the same index, that just means they're in
// different blocks.
case class Locals(
    // This doesn't have all the locals that are in the locals list, this just
    // has any locals added by templar.
    templarLocals: Map[VariableId2, VariableIdH],

    unstackifiedVars: Set[VariableIdH],

    // This has all the locals for the function, a superset of templarLocals.
    locals: Map[VariableIdH, Local]) {

  def addTemplarLocal(
    varId2: VariableId2,
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

  def markUnstackified(varId2: VariableId2): Locals = {
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

  def get(varId: VariableId2): Option[Local] = {
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
    val hamuts0 = Hamuts(Map(), Map(), Map(), hinputs.structIds.values.max + 1, Map(), Map(), Map(), Map())
    val (hamuts1, emptyPackStructRefH) =
      StructHammer.translateStructRef(hinputs, hamuts0, hinputs.program2.emptyPackStructRef)
    val hamuts2 = StructHammer.translateInterfaces(hinputs, hamuts1);
    val hamutsH = StructHammer.translateStructs(hinputs, hamuts2)
    val userFunctions = hinputs.program2.functions.filter(_.header.isUserFunction).toList
    val nonUserFunctions = hinputs.program2.functions.filter(!_.header.isUserFunction).toList
    val (hamuts4, _) = FunctionHammer.translateFunctions(hinputs, hamutsH, userFunctions)
    val (hamuts5, _) = FunctionHammer.translateFunctions(hinputs, hamuts4, nonUserFunctions)

    ProgramH(
      hamuts5.interfaceDefs.values.toList,
      hamuts5.structDefsById.values.toList,
      emptyPackStructRefH,
      List() /* externs */,
      hamuts5.functionDefs.values.toList)
  }

  def newId(nodesByLine: Vector[NodeH]) = {
    "" + nodesByLine.size
  }
}
