package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{BFunctionA, FunctionA, INameA, LambdaNameA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser._
import net.verdagon.vale.{scout, vassert, vimpl}
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env._

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
      temputs: TemputsBox,
      name: INameA) = {
    val (variability2, memberType) =
      env.getVariable(NameTranslator.translateVarNameStep(name)).get match {
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
      temputs: TemputsBox,
      containingFunctionEnv: FunctionEnvironment,
      name: LambdaNameA,
      function1: BFunctionA):
  (StructRef2) = {

    val closuredNames = function1.body.closuredNames;

    // Note, this is where the unordered closuredNames set becomes ordered.
    val closuredVarNamesAndTypes =
      closuredNames
        .map(name => determineClosureVariableMember(containingFunctionEnv, temputs, name))
        .toList;

    val (structRef, _, functionTemplata) =
      StructTemplar.makeClosureUnderstruct(
        containingFunctionEnv, temputs, name, function1.origin, closuredVarNamesAndTypes)

    // Eagerly evaluate the function if it's not a template.
    if (function1.origin.isTemplate) {
    // Do nothing
    } else {
      val unevaluatedContainers = vimpl()
      val _ =
        FunctionTemplar.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
          functionTemplata.outerEnv, temputs, structRef, unevaluatedContainers, function1.origin)
    }

    (structRef)
  }

  def evaluateOrdinaryFunctionFromNonCallForHeader(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata):
  FunctionHeader2 = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    if (function.isLight) {
      evaluateOrdinaryLightFunctionFromNonCallForHeader(
        env, temputs, unevaluatedContainers, function)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      val header =
        evaluateOrdinaryClosureFunctionFromNonCallForHeader(
          env, temputs, closureStructRef, unevaluatedContainers, function)
      header
    }
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryFunctionFromNonCallForPrototype(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata):
  (Prototype2) = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    if (function.isLight) {
      evaluateOrdinaryLightFunctionFromNonCallForPrototype(
        env, temputs, unevaluatedContainers, function)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      val header =
        evaluateOrdinaryClosureFunctionFromNonCallForHeader(
          env, temputs, closureStructRef, unevaluatedContainers, function)
      (header.toPrototype)
    }
  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata):
  (FunctionBanner2) = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    if (function.isLight()) {
      evaluateOrdinaryLightFunctionFromNonCallForBanner(
        env, temputs, unevaluatedContainers, function)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      evaluateOrdinaryClosureFunctionFromNonCallForBanner(
        env, temputs, closureStructRef, unevaluatedContainers, function)
    }
  }

  private def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      env: IEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionBanner2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForBanner(
      env, temputs, unevaluatedContainers, function)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    if (function.isLight()) {
      evaluateTemplatedLightFunctionFromCallForBanner(
        temputs, functionTemplata, alreadySpecifiedTemplateArgs, paramFilters)
    } else {
      val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
        env.getNearestTemplataWithName(
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
          Set(TemplataLookupContext))
      val banner =
        evaluateTemplatedClosureFunctionFromCallForBanner(
          env, temputs, closureStructRef, unevaluatedContainers, function, alreadySpecifiedTemplateArgs, paramFilters)
      (banner)
    }
  }

  private def evaluateTemplatedClosureFunctionFromCallForBanner(
      env: IEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarEnvLayer.evaluateTemplatedClosureFunctionFromCallForBanner(
      env, temputs, closureStructRef, unevaluatedContainers, function,
      alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    FunctionTemplarEnvLayer.evaluateTemplatedLightFunctionFromCallForBanner(
      env, temputs, unevaluatedContainers, function, alreadySpecifiedTemplateArgs, paramFilters)
  }

  private def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      env: IEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionHeader2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      env, temputs, closureStructRef, unevaluatedContainers, function)
  }

  private def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    env: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionBanner2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryClosureFunctionFromNonCallForBanner(
      env, temputs, closureStructRef, unevaluatedContainers, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  private def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      env: IEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (Prototype2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      env, temputs, unevaluatedContainers, function)
  }

  private def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      env: IEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionHeader2) = {
    FunctionTemplarEnvLayer.evaluateOrdinaryLightFunctionFromNonCallForHeader(
      env, temputs, unevaluatedContainers, function)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForTemputs(
      temputs: TemputsBox,
      functionTemplata: FunctionTemplata):
  Unit = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    val _ =
      evaluateOrdinaryLightFunctionFromNonCallForHeader(
        env, temputs, unevaluatedContainers, function)
  }

  def evaluateTemplatedFunctionFromCallForPrototype(
    temputs: TemputsBox,
    functionTemplata: FunctionTemplata,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  IEvaluateFunctionResult[Prototype2] = {
    val FunctionTemplata(env, unevaluatedContainers, function) = functionTemplata
    if (function.isLight()) {
      evaluateTemplatedLightFunctionFromCallForPrototype(
        env, temputs, unevaluatedContainers, function, explicitTemplateArgs, args)
    } else {
      evaluateTemplatedClosureFunctionFromCallForPrototype(
        env, temputs, unevaluatedContainers, function, explicitTemplateArgs, args)
    }
  }

  private def evaluateTemplatedLightFunctionFromCallForPrototype(
      env: IEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  IEvaluateFunctionResult[Prototype2] = {
    FunctionTemplarEnvLayer.evaluateTemplatedLightFunctionFromCallForPrototype(
        env, temputs, unevaluatedContainers, function, explicitTemplateArgs, args)
  }

  private def evaluateTemplatedClosureFunctionFromCallForPrototype(
    env: IEnvironment,
    temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  IEvaluateFunctionResult[Prototype2] = {
    val Some(KindTemplata(closureStructRef @ StructRef2(_))) =
      env.getNearestTemplataWithName(
        FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME,
        Set(TemplataLookupContext))
    FunctionTemplarEnvLayer.evaluateTemplatedClosureFunctionFromCallForPrototype(
      env, temputs, closureStructRef, unevaluatedContainers, function, explicitTemplateArgs, args)
  }
}
