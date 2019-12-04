package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.{BFunctionA, FunctionA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.{scout, vassert}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env._

import scala.collection.immutable.{List, Set}

// When templaring a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
object FunctionTemplar {
  trait IEvaluateFunctionResult[T]
  case class EvaluateFunctionSuccess[T](function: T) extends IEvaluateFunctionResult[T]
  case class EvaluateFunctionFailure[T](reason: String) extends IEvaluateFunctionResult[T]

  private def determineClosureVariableMember(
      env: FunctionEnvironment,
      temputs0: Temputs,
      name: String) = {
    val (variability2, memberType) =
      env.getVariable(name).get match {
        case ReferenceLocalVariable2(_, variability, reference) => {
          // See "Captured own is borrow" test for why we do this
          val tyype =
            reference.ownership match {
              case Own => ReferenceMemberType2(Coord(Borrow, reference.referend))
              case Borrow | Share => ReferenceMemberType2(reference)
            }
          (variability, tyype)
        }
        case AddressibleLocalVariable2(_, variability, reference) => {
          (variability, AddressMemberType2(reference))
        }
        case ReferenceClosureVariable2(_, _, variability, reference) => {
          // See "Captured own is borrow" test for why we do this
          val tyype =
            reference.ownership match {
              case Own => ReferenceMemberType2(Coord(Borrow, reference.referend))
              case Borrow | Share => ReferenceMemberType2(reference)
            }
          (variability, tyype)
        }
        case AddressibleClosureVariable2(_, _, variability, reference) => {
          (variability, AddressMemberType2(reference))
        }
      }
    StructMember2(name, variability2, memberType)
  }

  def evaluateClosureStruct(
      temputs0: Temputs,
      containingFunctionEnv: FunctionEnvironment,
      function1: BFunctionA):
  (Temputs, StructRef2) = {

    val closuredNames = function1.body.closuredNames;

    // Note, this is where the unordered closuredNames set becomes ordered.
    val closuredVarNamesAndTypes =
      closuredNames
        .map(name => determineClosureVariableMember(containingFunctionEnv, temputs0, name))
        .toList;

    val (temputs2, structRef, _, functionTemplata) =
      StructTemplar.makeClosureUnderstruct(
        containingFunctionEnv, temputs0, function1.origin, containingFunctionEnv.fullName, closuredVarNamesAndTypes)

    // Eagerly evaluate the function if it's not a template.
    val temputs4 =
      if (function1.origin.isTemplate) {
        temputs2
      } else {
        val (temputs3, _) =
          FunctionTemplar.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
            functionTemplata.outerEnv, temputs2, structRef, functionTemplata.function)
        temputs3
      }

    (temputs4, structRef)
  }

  def evaluateOrdinaryFunctionFromNonCallForPrototype(
    temputs0: Temputs,
    functionTemplata: FunctionTemplata):
  (Temputs, Prototype2) = {
    val FunctionTemplata(env, function1) = functionTemplata
    if (function1.isLight) {
      evaluateOrdinaryLightFunctionFromNonCallForPrototype(
        env, temputs0, function1)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      val (temputs1, header) =
        evaluateOrdinaryClosureFunctionFromNonCallForHeader(
          env, temputs0, closureStructRef, function1)
      (temputs1, header.toPrototype)
    }
  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
    temputs0: Temputs,
    functionTemplata: FunctionTemplata):
  (Temputs, FunctionBanner2) = {
    val FunctionTemplata(env, function1) = functionTemplata
    if (function1.isLight()) {
      evaluateOrdinaryLightFunctionFromNonCallForBanner(
        env, temputs0, function1)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      evaluateOrdinaryClosureFunctionFromNonCallForBanner(
        env, temputs0, closureStructRef, function1)
    }
  }

  private def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      env: IEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForBanner(
      env, temputs0, function1)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
    temputs0: Temputs,
    functionTemplata: FunctionTemplata,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    paramFilters: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    val FunctionTemplata(env, function1) = functionTemplata
    if (function1.isLight()) {
      evaluateTemplatedLightFunctionFromCallForBanner(
        temputs0, functionTemplata, alreadySpecifiedTemplateArgs, paramFilters)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      val (temputs1, banner) =
        evaluateTemplatedClosureFunctionFromCallForBanner(
          env, temputs0, closureStructRef, function1, alreadySpecifiedTemplateArgs, paramFilters)
      (temputs1, banner)
    }
  }

  private def evaluateTemplatedClosureFunctionFromCallForBanner(
      env: IEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarEnvLayer.evaluateTemplatedClosureFunctionFromCallForBanner(
      env, temputs0, closureStructRef, function,
      alreadySpecifiedTemplateArgs,argTypes2)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
    temputs0: Temputs,
    functionTemplata: FunctionTemplata,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    paramFilters: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    val FunctionTemplata(env, function) = functionTemplata
    FunctionTemplarEnvLayer.evaluateTemplatedLightFunctionFromCallForBanner(
      env, temputs0, function, alreadySpecifiedTemplateArgs, paramFilters)
  }

  private def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      env: IEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      function1: FunctionA):
  (Temputs, FunctionHeader2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      env, temputs0, closureStructRef, function1)
  }

  private def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    env: IEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryClosureFunctionFromNonCallForBanner(
      env, temputs0, closureStructRef, function1)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  private def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      env: IEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, Prototype2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      env, temputs0, function1)
  }

  private def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      env: IEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, FunctionHeader2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForHeader(
      env, temputs0, function1)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForTemputs(
      temputs0: Temputs,
      functionTemplata: FunctionTemplata):
  Temputs = {
    val FunctionTemplata(env, function1) = functionTemplata
    val (temputs1, _) =
      evaluateOrdinaryLightFunctionFromNonCallForHeader(
        env, temputs0, function1)
    temputs1
  }

  def evaluateTemplatedFunctionFromCallForPrototype(
    temputs0: Temputs,
    functionTemplata: FunctionTemplata,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    val FunctionTemplata(env, function) = functionTemplata
    if (function.isLight()) {
      evaluateTemplatedLightFunctionFromCallForPrototype(
        env, temputs0, function, explicitTemplateArgs, args)
    } else {
      evaluateTemplatedClosureFunctionFromCallForPrototype(
        env, temputs0, function, explicitTemplateArgs, args)
    }
  }

  private def evaluateTemplatedLightFunctionFromCallForPrototype(
      env: IEnvironment,
      temputs0: Temputs,
      function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    FunctionTemplarEnvLayer.evaluateTemplatedLightFunctionFromCallForPrototype(
        env, temputs0, function, explicitTemplateArgs, args)
  }

  private def evaluateTemplatedClosureFunctionFromCallForPrototype(
    env: IEnvironment,
    temputs0: Temputs,
    function: FunctionA,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
      env.getNearestTemplataWithName(
        FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
        Set(TemplataLookupContext))
    FunctionTemplarEnvLayer.evaluateTemplatedClosureFunctionFromCallForPrototype(
      env, temputs0, closureStructRef, function, explicitTemplateArgs, args)
  }
}
