package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, FunctionA, IRulexAR, IRuneA, ITemplataType}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.FunctionTemplar.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
import net.verdagon.vale.templar.infer.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.{vcurious, vimpl}
//import net.verdagon.vale.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.templar.templata.TemplataTemplar
import net.verdagon.vale.{vassert, vfail, vwat}

import scala.collection.immutable.{List, Set}

// When templaring a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
object FunctionTemplarOrdinaryOrTemplatedLayer {
  // This is for the early stages of Templar when it's scanning banners to put in
  // its env. We just want its banner, we don't want to evaluate it.
  def predictOrdinaryFunctionBanner(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, function)

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs, rules, typeByRune)
    val runedEnv = addRunedDataToNearEnv(nearEnv, List(), inferences, function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.predictOrdinaryFunctionBanner(runedEnv, temputs, function)
  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, function)
    vassert(!EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs, rules, typeByRune)
    val runedEnv = addRunedDataToNearEnv(nearEnv, List(), inferences, function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(runedEnv, temputs, function)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
      argTypes2: List[Coord]):
  (FunctionHeader2) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, function)
    vassert(EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val maybeInferredTemplatas =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs,
        function.identifyingRunes,
        rules,
        typeByRune,
        function.params.map(_.pattern),
        function.maybeRetCoordRune,
        List(),
        argTypes2.map(arg => ParamFilter(arg, None)))
    val InferSolveSuccess(inferredTemplatas) = maybeInferredTemplatas

    val runedEnv =
      addRunedDataToNearEnv(
        nearEnv,
        function.identifyingRunes,
        inferredTemplatas.templatasByRune,
        function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(runedEnv, temputs, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, function)
    vassert(EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferredTemplatas =
      InferTemplar.inferFromArgCoords(
          nearEnv,
          temputs,
          function.identifyingRunes,
          rules,
          typeByRune,
          function.params.map(_.pattern),
          function.maybeRetCoordRune,
          explicitTemplateArgs,
          args) match {
        case (isf @ InferSolveFailure(_, _, _, _, _, _)) => {
          return (EvaluateFunctionFailure(isf.toString))
        }
        case (InferSolveSuccess(i)) => (i)
      }

    val runedEnv = addRunedDataToNearEnv(nearEnv, function.identifyingRunes, inferredTemplatas.templatasByRune, function.maybeRetCoordRune)

    val prototype =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
        runedEnv, temputs, function)
    (EvaluateFunctionSuccess(prototype))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForBanner(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, function)
    vassert(EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferredTemplatas =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs,
        function.identifyingRunes,
        rules,
        typeByRune,
        function.params.map(_.pattern),
        function.maybeRetCoordRune,
        alreadySpecifiedTemplateArgs,
        paramFilters) match {
      case (isf @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (EvaluateFunctionFailure(isf.toString))
      }
      case (InferSolveSuccess(i)) => (i)
    }

    val runedEnv =
      addRunedDataToNearEnv(
        nearEnv, function.identifyingRunes, inferredTemplatas.templatasByRune, function.maybeRetCoordRune)

    val banner =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        runedEnv, temputs, function)
    (EvaluateFunctionSuccess(banner))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateTemplatedFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionHeader2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, function)
    vassert(EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    vassert(function.identifyingRunes.size == List().size);

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)

    val result =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv,
        temputs,
        function.identifyingRunes,
        rules,
        typeByRune,
        function.params.map(_.pattern),
        function.maybeRetCoordRune,
        List())
    val inferences =
      result match {
        case isf @ InferSolveFailure(_, _, _, _, _, _) => {
          vfail("Couldnt figure out template args! Cause:\n" + isf)
        }
        case InferSolveSuccess(i) => i
      }

    val runedEnv = addRunedDataToNearEnv(nearEnv, List(), inferences.templatasByRune, function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(runedEnv, temputs, function)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateOrdinaryFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionHeader2) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, function)
    vassert(!EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))


    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs, rules, typeByRune)
    val runedEnv = addRunedDataToNearEnv(nearEnv, List(), inferences, function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(
      runedEnv, temputs, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryFunctionFromNonCallForPrototype(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (Prototype2) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, function)
    vassert(!EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs, rules, typeByRune)
    val runedEnv = addRunedDataToNearEnv(nearEnv, List(), inferences, function.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
      runedEnv, temputs, function)
  }


  // This is called while we're trying to figure out what functionSs to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs: TemputsBox,
    unevaluatedContainers: List[IContainer],
    function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(EnvironmentUtils.functionIsTemplateInContext(unevaluatedContainers, function))

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val inferences =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs,
        function.identifyingRunes,
        rules,
        typeByRune,
        function.params.map(_.pattern),
        function.maybeRetCoordRune,
        explicitTemplateArgs,
        args) match {
      case (isc @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (EvaluateFunctionFailure[FunctionBanner2](isc.toString))
      }
      case (InferSolveSuccess(inferredTemplatas)) => (inferredTemplatas.templatasByRune)
    }

    // See FunctionTemplar doc for what outer/runes/inner envs are.
    val runedEnv = addRunedDataToNearEnv(nearEnv, function.identifyingRunes, inferences, function.maybeRetCoordRune)

    val banner =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        runedEnv, temputs, function)

    (EvaluateFunctionSuccess(banner))
  }

  def scanOrdinaryInterfaceMember(
    nearEnv: FunctionEnvironment,
    temputs: TemputsBox,
    interfaceExplicitTemplateArgs: List[ITemplata],
    unevaluatedContainers: List[IContainer],
    function: FunctionA):
  (FunctionHeader2) = {

    val (rules, typeByRune) =
      EnvironmentUtils.assembleRulesFromFunctionAndContainers(unevaluatedContainers, function)
    val result =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv,
        temputs,
        function.identifyingRunes,
        rules,
        typeByRune,
        function.params.map(_.pattern),
        function.maybeRetCoordRune,
        interfaceExplicitTemplateArgs)

    // A precondition of this call was that it was an ordinary function, which is why
    // maybeTemplataByRune is filled.
    val templataByRune =
      result match {
        case isf @ InferSolveFailure(_, _, _, _, _, _) => {
          vfail("Couldnt figure out template args! Cause:\n" + isf)
        }
        case InferSolveSuccess(i) => i.templatasByRune
      }

    val runedEnv = addRunedDataToNearEnv(nearEnv, function.identifyingRunes, templataByRune, function.maybeRetCoordRune)

    val params2 =
      FunctionTemplarMiddleLayer.assembleFunctionParams(
        runedEnv, temputs, function.params)

    val Some(CoordTemplata(retCoord)) =
      runedEnv.getNearestTemplataWithName(vimpl(function.maybeRetCoordRune.get.toString), Set(TemplataLookupContext))


    temputs.declareFunctionSignature(
      Signature2(runedEnv.fullName, params2.map(_.tyype)),
      Some(runedEnv))
    val header =
      FunctionTemplarCore.makeInterfaceFunction(
        runedEnv, temputs, Some(function), params2, retCoord)
    header
  }

  private def checkClosureConcernsHandled(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    function: FunctionA
  ): Unit = {
    function.body match {
      case CodeBodyA(body1) => {
        body1.closuredNames.foreach(name => {
          vimpl()//vassert(nearEnv.variables.exists(_.id.variableName == name))
        })
      }
      case _ =>
    }
  }

  // IOW, add the necessary data to turn the near env into the runed env.
  def addRunedDataToNearEnv(
    nearEnv: FunctionEnvironment,
    identifyingRunes: List[IRuneA],
    templatasByRune: Map[IRune2, ITemplata],
    maybeRetCoordRune: Option[IRuneA]):
  FunctionEnvironment = {

    val identifyingTemplatas = identifyingRunes.map(NameTranslator.translateRune).map(templatasByRune)

    vcurious(nearEnv.fullName.last.isInstanceOf[FunctionName2])
    val nearEnvFunctionName = nearEnv.fullName.last.asInstanceOf[FunctionName2];

    val fullName =
      FullName2(
        nearEnv.fullName.steps.init, nearEnvFunctionName.copy(templateArgs = identifyingTemplatas))
    val maybeReturnType = maybeRetCoordRune.map(retCoordRune => {
      templatasByRune.get(NameTranslator.translateRune(retCoordRune)) match {
        case Some(CoordTemplata(coord)) => coord
        case other => vwat(other.toString)
      }
    })
    nearEnv
        .copy(fullName = fullName, maybeReturnType = maybeReturnType)
        .addEntries(templatasByRune.map({
          case (k, v) => (k, List(TemplataEnvEntry(v)))
        })
        .toMap[IName2, List[IEnvEntry]])
  }
}
