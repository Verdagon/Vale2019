package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.CodeBody1
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{FunctionEnvironment, FunctionEnvironmentBox, IEnvironment}
import net.verdagon.vale.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.vale.{vassert, vfail}

import scala.collection.immutable.List

object FunctionTemplarEnvLayer {
  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
    containingEnv: IEnvironment, temputs: TemputsBox, function1: FunctionA):
  (FunctionBanner2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs,
      function1)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      functionS: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))

    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedFunctionFromCallForBanner(
      funcOuterEnv, temputs, functionS, alreadySpecifiedTemplateArgs, paramFilters)
  }


  def evaluateTemplatedClosureFunctionFromCallForBanner(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForBanner(
      funcOuterEnv, temputs, closureStructRef, functionS,
      alreadySpecifiedTemplateArgs, argTypes2)
  }


  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    functionS: FunctionA):
  (FunctionBanner2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs,
      closureStructRef,
      functionS)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
      functionS: FunctionA):
  (FunctionHeader2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs,
      closureStructRef,
      functionS)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      function1: FunctionA):
  (FunctionHeader2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs,
      function1)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    function1: FunctionA):
  (Prototype2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      funcOuterEnv,
      temputs,
      function1)
  }

//  // This is called while we're trying to figure out what function1s to call when there
//  // are a lot of overloads available.
//  // This assumes it met any type bound restrictions (or, will; not implemented yet)
//  def evaluateTemplatedLightBannerFromCall(
//      containingEnv: IEnvironment,
//      temputs: TemputsBox,
//      functionS: FunctionA,
//      explicitTemplateArgs: List[ITemplata],
//      args: List[ParamFilter]):
//  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
//
//    // See FunctionTemplar doc for what outer/runes/inner envs are.
//    val funcOuterEnv =
//      FunctionEnvironment(containingEnv, Map(), functionS, Set(), 0, List(), Set())
//
//    FunctionTemplarClosureOrLightLayer.evaluateTemplatedLightBannerFromCall(
//      funcOuterEnv, temputs, functionS, explicitTemplateArgs, args)
//  }

  def evaluateTemplatedLightFunctionFromCallForPrototype(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateTemplatedLightFunctionFromCallForPrototype2(
      funcOuterEnv, temputs, functionS, explicitTemplateArgs, args)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None, None, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForPrototype(
      funcOuterEnv, temputs, closureStructRef, functionS,
      alreadySpecifiedTemplateArgs, argTypes2)
  }
}
