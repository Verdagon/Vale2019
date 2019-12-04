package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout.CodeBody1
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment}
import net.verdagon.radonc.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object FunctionTemplarEnvLayer {
  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
    containingEnv: IEnvironment, temputs0: Temputs, function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs0,
      function1)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
      containingEnv: IEnvironment,
      temputs0: Temputs,
      functionS: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))

    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedFunctionFromCallForBanner(
      funcOuterEnv, temputs0, functionS, alreadySpecifiedTemplateArgs, paramFilters)
  }


  def evaluateTemplatedClosureFunctionFromCallForBanner(
    containingEnv: IEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForBanner(
      funcOuterEnv, temputs0, closureStructRef, functionS,
      alreadySpecifiedTemplateArgs, argTypes2)
  }


  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    containingEnv: IEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    functionS: FunctionA):
  (Temputs, FunctionBanner2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs0,
      closureStructRef,
      functionS)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      functionS: FunctionA):
  (Temputs, FunctionHeader2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs0,
      closureStructRef,
      functionS)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, FunctionHeader2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs0,
      function1)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    containingEnv: IEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, Prototype2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(function1.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function1, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      funcOuterEnv,
      temputs0,
      function1)
  }

//  // This is called while we're trying to figure out what function1s to call when there
//  // are a lot of overloads available.
//  // This assumes it met any type bound restrictions (or, will; not implemented yet)
//  def evaluateTemplatedLightBannerFromCall(
//      containingEnv: IEnvironment,
//      temputs0: Temputs,
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
//      funcOuterEnv, temputs0, functionS, explicitTemplateArgs, args)
//  }

  def evaluateTemplatedLightFunctionFromCallForPrototype(
      containingEnv: IEnvironment,
      temputs0: Temputs,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateTemplatedLightFunctionFromCallForPrototype2(
      funcOuterEnv, temputs0, functionS, explicitTemplateArgs, args)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    containingEnv: IEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps :+ NamePart2(functionS.name, None))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, functionS, Map(), None, Set(), 0, List(), Set())

    functionS.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForPrototype(
      funcOuterEnv, temputs0, closureStructRef, functionS,
      alreadySpecifiedTemplateArgs, argTypes2)
  }

}
