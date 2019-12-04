package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.FunctionTemplar.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.templar.templata.TemplataTemplar
import net.verdagon.radonc.{vassert, vfail, vwat}

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
    temputs0: Temputs,
    functionA: FunctionA):
  (Temputs, FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, functionA)

    val (temputs1, inferences) =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs0, functionA.templateRules, functionA.typeByRune)
    val innerEnv = makeInnerEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.predictOrdinaryFunctionBanner(innerEnv, temputs1, functionA)
  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs0: Temputs, functionA: FunctionA):
  (Temputs, FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val (temputs1, inferences) =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs0, functionA.templateRules, functionA.typeByRune)
    val innerEnv = makeInnerEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(innerEnv, temputs1, functionA)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionA: FunctionA,
      argTypes2: List[Coord]):
  (Temputs, FunctionHeader2) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val (temputs1, maybeInferredTemplatas) =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs0,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        List(),
        argTypes2.map(arg => ParamFilter(arg, None)))
    val InferSolveSuccess(inferredTemplatas) = maybeInferredTemplatas

    val innerEnv = makeInnerEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)
    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(innerEnv, temputs1, functionA)
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
    temputs0: Temputs,
    functionA: FunctionA,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val (temputs2, inferredTemplatas) =
      InferTemplar.inferFromArgCoords(
          nearEnv,
          temputs0,
          functionA.identifyingRunes,
          functionA.templateRules,
          functionA.typeByRune,
          functionA.params.map(_.pattern),
          functionA.maybeRetCoordRune,
          explicitTemplateArgs,
          args) match {
        case (temputs1, isf @ InferSolveFailure(_, _, _, _, _, _)) => {
          return (temputs1, EvaluateFunctionFailure(isf.toString))
        }
        case (temputs1, InferSolveSuccess(i)) => (temputs1, i)
      }

    val innerEnv = makeInnerEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)

    val (temputs3, prototype) =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
        innerEnv, temputs2, functionA)
    (temputs3, EvaluateFunctionSuccess(prototype))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForBanner(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionA: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val (temputs2, inferredTemplatas) =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs0,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        alreadySpecifiedTemplateArgs,
        paramFilters) match {
      case (temputs1, isf @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (temputs1, EvaluateFunctionFailure(isf.toString))
      }
      case (temputs1, InferSolveSuccess(i)) => (temputs1, i)
    }

    val innerEnv = makeInnerEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)

    val (temputs10, banner) =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        innerEnv, temputs2, functionA)
    (temputs10, EvaluateFunctionSuccess(banner))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateTemplatedFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionA: FunctionA):
  (Temputs, FunctionHeader2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    vassert(functionA.identifyingRunes.size == List().size);

    val (temputs1, result) =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv,
        temputs0,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        List())
    val inferences =
      result match {
        case isf @ InferSolveFailure(_, _, _, _, _, _) => {
          vfail("Couldnt figure out template args! Cause:\n" + isf)
        }
        case InferSolveSuccess(i) => i
      }

    val innerEnv = makeInnerEnv(nearEnv, List(), inferences.templatasByRune, functionA.maybeRetCoordRune)
    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(innerEnv, temputs1, functionA)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateOrdinaryFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionA: FunctionA):
  (Temputs, FunctionHeader2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val (temputs1, inferences) =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs0, functionA.templateRules, functionA.typeByRune)
    val innerEnv = makeInnerEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(
      innerEnv, temputs1, functionA)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryFunctionFromNonCallForPrototype(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    temputs0: Temputs,
    functionA: FunctionA):
  (Temputs, Prototype2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val (temputs1, inferences) =
      InferTemplar.inferOrdinaryRules(
        nearEnv, temputs0, functionA.templateRules, functionA.typeByRune)
    val innerEnv = makeInnerEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
      innerEnv, temputs1, functionA)
  }


  // This is called while we're trying to figure out what functionSs to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionA: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    functionA.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(functionA.isTemplate)


    val (temputs2, inferences) =
      InferTemplar.inferFromArgCoords(
        nearEnv,
        temputs0,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        explicitTemplateArgs,
        args) match {
      case (temputs1, isc @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (temputs1, EvaluateFunctionFailure[FunctionBanner2](isc.toString))
      }
      case (temputs1, InferSolveSuccess(inferredTemplatas)) => (temputs1, inferredTemplatas.templatasByRune)
    }

    // See FunctionTemplar doc for what outer/runes/inner envs are.
    val innerEnv = makeInnerEnv(nearEnv, functionA.identifyingRunes, inferences, functionA.maybeRetCoordRune)

    val (temputs3, banner) =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        innerEnv, temputs2, functionA)

    (temputs3, EvaluateFunctionSuccess(banner))
  }

  def scanOrdinaryInterfaceMember(
    nearEnv: FunctionEnvironment,
    temputs0: Temputs,
    interfaceExplicitTemplateArgs: List[ITemplata],
    functionA: FunctionA):
  (Temputs, FunctionHeader2) = {
    val (temputs1, result) =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv,
        temputs0,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
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

    val innerEnv = makeInnerEnv(nearEnv, functionA.identifyingRunes, templataByRune, functionA.maybeRetCoordRune)

    val (temputs2, params2) =
      FunctionTemplarMiddleLayer.assembleFunctionParams(
        innerEnv, temputs1, functionA.params)

    val Some(CoordTemplata(retCoord)) =
      innerEnv.getNearestTemplataWithName(functionA.maybeRetCoordRune.get, Set(TemplataLookupContext))

    val temputs8 =
      temputs2.declareFunctionSignature(
        Signature2(innerEnv.fullName, params2.map(_.tyype)),
        Some(innerEnv))
    val (temputs9, header) =
      FunctionTemplarCore.makeInterfaceFunction(
        innerEnv, temputs8, Some(functionA), params2, retCoord)

    (temputs9, header)
  }

  private def checkClosureConcernsHandled(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironment,
    functionA: FunctionA
  ): Unit = {
    functionA.body match {
      case CodeBodyA(body1) => {
        body1.closuredNames.foreach(name => {
          vassert(nearEnv.variables.exists(_.id.variableName == name))
        })
      }
      case _ =>
    }
  }

  def makeInnerEnv(
    nearEnv: FunctionEnvironment,
    identifyingRunes: List[String],
    templatasByRune: Map[String, ITemplata],
    maybeRetCoordRune: Option[String]):
  FunctionEnvironment = {
    val identifyingTemplatas = identifyingRunes.map(templatasByRune)
    val fullName = FullName2(nearEnv.fullName.steps.init :+ nearEnv.fullName.steps.last.copy(templateArgs = Some(identifyingTemplatas)))
    val maybeReturnType = maybeRetCoordRune.map(retCoordRune => {
      templatasByRune.get(retCoordRune) match {
        case Some(CoordTemplata(coord)) => coord
        case _ => vwat()
      }
    })
    nearEnv
      // Change the fullName's templateArgs from None to Some(List()) to be consistent with
      // the rest of the layer.
      .copy(fullName = fullName)
      .copy(maybeReturnType = maybeReturnType)
      .addEntries(templatasByRune.mapValues(x => List(TemplataEnvEntry(x))))
  }
}
