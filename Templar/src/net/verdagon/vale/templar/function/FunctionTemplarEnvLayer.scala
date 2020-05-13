package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, ConstructorNameA, FunctionA, FunctionNameA, IFunctionDeclarationNameA, LambdaNameA, TopLevelCitizenDeclarationNameA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.CodeBody1
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment, FunctionEnvironmentBox, IEnvironment}
import net.verdagon.vale.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.vale.{vassert, vfail, vimpl}

import scala.collection.immutable.List

object FunctionTemplarEnvLayer {
  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    function: FunctionA):
  (FunctionBanner2) = {
    val functionNamePart = translateFunctionDeclarationName(function.name)
    val functionFullName = FullName2(containingEnv.fullName.steps, functionNamePart)
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs,

      function)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      function: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps, translateFunctionDeclarationName(function.name))

    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())

    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedFunctionFromCallForBanner(
      funcOuterEnv, temputs,

      function, alreadySpecifiedTemplateArgs, paramFilters)
  }


  def evaluateTemplatedClosureFunctionFromCallForBanner(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps, translateFunctionDeclarationName(function.name))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())

    function.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForBanner(
      funcOuterEnv, temputs, closureStructRef,

      function,
      alreadySpecifiedTemplateArgs, argTypes2)
  }


  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function: FunctionA):
  (FunctionBanner2) = {
    val functionFullName = containingEnv.fullName.addStep(translateFunctionDeclarationName(function.name))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForBanner(
      funcOuterEnv,
      temputs,
      closureStructRef,

      function)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
      function: FunctionA):
  (FunctionHeader2) = {
    val functionFullName = containingEnv.fullName.addStep(translateFunctionDeclarationName(function.name))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())

    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs,
      closureStructRef,

      function)
  }

  // We receive a FunctionEnvEntry here just to be consistent with the other methods.
  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      containingEnv: IEnvironment,
      temputs: TemputsBox,
      function: FunctionA):
  (FunctionHeader2) = {
    val functionFullName = FullName2(containingEnv.fullName.steps, translateFunctionDeclarationName(function.name))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForHeader(
      funcOuterEnv,
      temputs,
      function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    function: FunctionA):
  (Prototype2) = {

    val functionNamePart = translateFunctionDeclarationName(function.name)
    val functionFullName =
      FullName2(containingEnv.fullName.steps, functionNamePart)
    val funcOuterEnv =
      FunctionEnvironment(
        containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      funcOuterEnv,
      temputs,

      function)
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
      function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    val functionFullName = FullName2(containingEnv.fullName.steps, translateFunctionDeclarationName(function.name))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())
    FunctionTemplarClosureOrLightLayer.evaluateTemplatedLightFunctionFromCallForPrototype2(
      funcOuterEnv, temputs,

      function, explicitTemplateArgs, args)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    containingEnv: IEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {

    val functionFullName = FullName2(containingEnv.fullName.steps, vimpl(function.name.toString))
    val funcOuterEnv =
      FunctionEnvironment(containingEnv, functionFullName, function, Map(), None, List(), 0, List(), Set())

    function.body match {
      case CodeBodyA(body1) => vassert(!body1.closuredNames.isEmpty)
      case _ => vfail()
    }

    FunctionTemplarClosureOrLightLayer.evaluateTemplatedClosureFunctionFromCallForPrototype(
      funcOuterEnv,
      temputs,
      closureStructRef,

      function,
      alreadySpecifiedTemplateArgs,
      argTypes2)
  }

  def translateFunctionDeclarationName(functionNameA: IFunctionDeclarationNameA): IFunctionName2 = {
      functionNameA match {
        case FunctionNameA(humanName, codeLocationS) => FunctionName2(humanName, List(), List())
        case LambdaNameA(codeLocation) => FunctionName2(CallTemplar.CALL_FUNCTION_NAME, List(), List())
        case ConstructorNameA(TopLevelCitizenDeclarationNameA(name, codeLocation)) => FunctionName2(name, List(), List())
        case _ => vimpl()
      }
  }
}
