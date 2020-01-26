package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.LocalVariable1
import net.verdagon.vale.templar.templata.{ITemplata, Queriable2}
import net.verdagon.vale.templar.types.{Coord, FullName2, StructRef2, Variability}
import net.verdagon.vale.{vassert, vfail, vimpl}

import scala.collection.immutable.{List, Map, Set}

case class FunctionEnvironment(
  // These things are the "environment"; they are the same for every line in a function.
  parentEnv: IEnvironment,
  fullName: FullName2, // Includes the name of the function
  function: FunctionA,
  entries: Map[AbsoluteNameA[INameA], List[IEnvEntry]],
  maybeReturnType: Option[Coord],

  // The scout information for locals for this block and all parent blocks in this function.
  scoutedLocals: Set[LocalVariableA],

  // The things below are the "state"; they can be different for any given line in a function.
  varCounter: Int,
  variables: List[IVariable2],
  moveds: Set[VariableId2]

  // We just happen to combine these two things into one FunctionEnvironment.
  // It might even prove useful one day... since the StructDef for a lambda remembers
  // its original environment, a closure can know all the variable IDs and moveds for
  // its containing function at that time.
  // See AENS for some more thoughts on environment vs state.

) extends IEnvironment {
  vassert(fullName.steps.startsWith(parentEnv.fullName.steps))

  override def globalEnv: NamespaceEnvironment = parentEnv.globalEnv

  def addScoutedLocals(newScoutedLocals: Set[LocalVariableA]): FunctionEnvironment = {
    FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals ++ newScoutedLocals, varCounter, variables, moveds)
  }
  def addVariables(newVars: List[IVariable2]): FunctionEnvironment = {
    FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter, variables ++ newVars, moveds)
  }
  def addVariable(newVar: IVariable2): FunctionEnvironment = {
    FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter, variables :+ newVar, moveds)
  }
  def markVariablesMoved(newMoveds: Set[VariableId2]): FunctionEnvironment = {
    newMoveds.foldLeft(this)({
      case (intermediateFate, newMoved) => intermediateFate.markVariableMoved(newMoved)
    })
  }
  def markVariableMoved(newMoved: VariableId2): FunctionEnvironment = {
    if (variables.exists(_.id == newMoved)) {
      FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter, variables, moveds + newMoved)
    } else {
      val parentFuncEnv =
        parentEnv match { case f @ FunctionEnvironment(_, _, _, _, _, _, _, _, _) => f case _ => vfail() }
      val newParent = parentFuncEnv.markVariableMoved(newMoved)
      FunctionEnvironment(newParent, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter, variables, moveds)
    }
  }
  def nextVarCounter(): (FunctionEnvironment, Int) = {
    (FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter + 1, variables, moveds), varCounter)
  }
  // n is how many values to get
  def nextCounters(n: Int): (FunctionEnvironment, List[Int]) = {
    (
      FunctionEnvironment(parentEnv, fullName, function, entries, maybeReturnType, scoutedLocals, varCounter + n, variables, moveds),
      (0 until n).map(_ + varCounter).toList)
  }

  def addEntry(name: AbsoluteNameA[INameA], entry: IEnvEntry): FunctionEnvironment = {
    FunctionEnvironment(
      parentEnv,
      fullName,
      function,
      EnvironmentUtils.addEntry(entries, name, entry),
      maybeReturnType,
      scoutedLocals,
      varCounter,
      variables,
      moveds)
  }
  def addEntries(newEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]]): FunctionEnvironment = {
    FunctionEnvironment(
      parentEnv,
      fullName,
      function,
      EnvironmentUtils.addEntries(entries, newEntries),
      maybeReturnType,
      scoutedLocals,
      varCounter,
      variables,
      moveds)
  }

  override def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.getOrElse(name, List())
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter))
      .map(EnvironmentUtils.entryToTemplata(this, _)) ++
      parentEnv.getAllTemplatasWithAbsoluteName(name, lookupFilter)
  }

  override def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries
      .get(name).toList.flatten
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter)) match {
      case List(entry) => Some(EnvironmentUtils.entryToTemplata(this, entry))
      case List() => parentEnv.getNearestTemplataWithAbsoluteName(name, lookupFilter)
      case multiple => vfail("Too many things named " + name + ":" + multiple);
    }
  }

  override def getAllTemplatasWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }

  override def getNearestTemplataWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
  }

  def getVariable(name: AbsoluteNameA[IVarNameA]): Option[IVariable2] = {
    variables.find(_.id.variableName == name) match {
      case Some(v) => Some(v)
      case None => {
        parentEnv match {
          case pfe @ FunctionEnvironment(_, _, _, _, _, _, _, _, _) => pfe.getVariable(name)
          case _ => None
        }
      }
    }
  }

  def getAllLiveLocals(): List[ILocalVariable2] = {
    val parentLiveLocals =
      parentEnv match {
        case parentFuncEnv @ FunctionEnvironment(_, _, _, _, _, _, _, _, _) => parentFuncEnv.getAllLiveLocals()
        case _ => List()
      }
    val liveLocals =
      variables.flatMap({
        case i : ILocalVariable2 => if (!moveds.contains(i.id)) List(i) else List()
        case _ => List()
      })
    parentLiveLocals ++ liveLocals
  }

  // No particular reason we don't have an addFunction like NamespaceEnvironment does
}

case class FunctionEnvironmentBox(var functionEnvironment: FunctionEnvironment) extends IEnvironmentBox {
  override def snapshot: FunctionEnvironment = functionEnvironment
  def parentEnv: IEnvironment = functionEnvironment.parentEnv
  def fullName: FullName2 = functionEnvironment.fullName
  def function: FunctionA = functionEnvironment.function
  def entries: Map[AbsoluteNameA[INameA], List[IEnvEntry]] = functionEnvironment.entries
  def maybeReturnType: Option[Coord] = functionEnvironment.maybeReturnType
  def scoutedLocals: Set[LocalVariableA] = functionEnvironment.scoutedLocals
  def varCounter: Int = functionEnvironment.varCounter
  def variables: List[IVariable2] = functionEnvironment.variables
  def moveds: Set[VariableId2] = functionEnvironment.moveds
  override def globalEnv: NamespaceEnvironment = parentEnv.globalEnv

  def setReturnType(returnType: Option[Coord]): Unit = {
    functionEnvironment = functionEnvironment.copy(maybeReturnType = returnType)
  }

  def setFullName(fullName: FullName2): Unit = {
    functionEnvironment = functionEnvironment.copy(fullName = fullName)
  }

  def addScoutedLocals(newScoutedLocals: Set[LocalVariableA]): Unit = {
    functionEnvironment = functionEnvironment.addScoutedLocals(newScoutedLocals)
  }
  def addVariables(newVars: List[IVariable2]): Unit= {
    functionEnvironment = functionEnvironment.addVariables(newVars)
  }
  def addVariable(newVar: IVariable2): Unit= {
    functionEnvironment = functionEnvironment.addVariable(newVar)
  }
  def markVariablesMoved(newMoveds: Set[VariableId2]): Unit= {
    functionEnvironment = functionEnvironment.markVariablesMoved(newMoveds)
  }
  def markVariableMoved(newMoved: VariableId2): Unit= {
    functionEnvironment = functionEnvironment.markVariableMoved(newMoved)
  }
  def nextVarCounter(): Int = {
    val (newFunctionEnvironment, varCounter) = functionEnvironment.nextVarCounter()
    functionEnvironment = newFunctionEnvironment
    varCounter
  }
  // n is how many values to get
  def nextCounters(n: Int): List[Int] = {
    val (newFunctionEnvironment, counters) = functionEnvironment.nextCounters(n)
    functionEnvironment = newFunctionEnvironment
    counters
  }

  def addEntry(name: AbsoluteNameA[INameA], entry: IEnvEntry): Unit = {
    functionEnvironment = functionEnvironment.addEntry(name, entry)
  }
  def addEntries(newEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]]): Unit= {
    functionEnvironment = functionEnvironment.addEntries(newEntries)
  }

  override def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    functionEnvironment.getAllTemplatasWithAbsoluteName(name, lookupFilter)
  }

  override def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    functionEnvironment.getNearestTemplataWithAbsoluteName(name, lookupFilter)
  }

  def getVariable(name: AbsoluteNameA[IVarNameA]): Option[IVariable2] = {
    functionEnvironment.getVariable(name)
  }

  def getAllLiveLocals(): List[ILocalVariable2] = {
    functionEnvironment.getAllLiveLocals()
  }

  // No particular reason we don't have an addFunction like NamespaceEnvironment does
}


case class VariableId2(
  lambdaNumber: Int,
  variableName: AbsoluteNameA[IVarNameA]) extends Queriable2 {

  println("hi!")

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class TemplarImplicitVarNameA(num: Int) extends IVarName2

sealed trait IVariable2 extends Queriable2 {
  def id: VariableId2
  def variability: Variability
  def reference: Coord
}
sealed trait ILocalVariable2 extends IVariable2
// Why the difference between reference and addressible:
// If we mutate/move a variable from inside a closure, we need to put
// the local's address into the struct. But, if the closures don't
// mutate/move, then we could just put a regular reference in the struct.
// Lucky for us, the parser figured out if any of our child closures did
// any mutates/moves/borrows.
case class AddressibleLocalVariable2(
  id: VariableId2,
  variability: Variability,
  reference: Coord
) extends ILocalVariable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ id.all(func) ++ variability.all(func) ++ reference.all(func)
  }
}
case class ReferenceLocalVariable2(
  id: VariableId2,
  variability: Variability,
  reference: Coord
) extends ILocalVariable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ id.all(func) ++ variability.all(func) ++ reference.all(func)
  }
}
case class AddressibleClosureVariable2(
  id: VariableId2,
  closuredVarsStructType: StructRef2,
  variability: Variability,
  reference: Coord
) extends IVariable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ id.all(func) ++ closuredVarsStructType.all(func) ++ variability.all(func) ++ reference.all(func)
  }
}
case class ReferenceClosureVariable2(
  id: VariableId2,
  closuredVarsStructType: StructRef2,
  variability: Variability,
  reference: Coord
) extends IVariable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ id.all(func) ++ closuredVarsStructType.all(func) ++ variability.all(func) ++ reference.all(func)
  }
}
