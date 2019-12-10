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
    nearEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    functionA: FunctionA):
  (FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, functionA)

    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv.snapshot, temputs, functionA.templateRules, functionA.typeByRune)
    addInnerDataToNearEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.predictOrdinaryFunctionBanner(innerEnv.snapshot, temputs, functionA)
  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironmentBox,
    temputs: TemputsBox, functionA: FunctionA):
  (FunctionBanner2) = {
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv.snapshot, temputs, functionA.templateRules, functionA.typeByRune)
    addInnerDataToNearEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(innerEnv, temputs, functionA)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionA: FunctionA,
      argTypes2: List[Coord]):
  (FunctionHeader2) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val maybeInferredTemplatas =
      InferTemplar.inferFromArgCoords(
        nearEnv.snapshot,
        temputs,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        List(),
        argTypes2.map(arg => ParamFilter(arg, None)))
    val InferSolveSuccess(inferredTemplatas) = maybeInferredTemplatas

    addInnerDataToNearEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(innerEnv, temputs, functionA)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    functionA: FunctionA,
    explicitTemplateArgs: List[ITemplata],
    args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val inferredTemplatas =
      InferTemplar.inferFromArgCoords(
          nearEnv.snapshot,
          temputs,
          functionA.identifyingRunes,
          functionA.templateRules,
          functionA.typeByRune,
          functionA.params.map(_.pattern),
          functionA.maybeRetCoordRune,
          explicitTemplateArgs,
          args) match {
        case (isf @ InferSolveFailure(_, _, _, _, _, _)) => {
          return (EvaluateFunctionFailure(isf.toString))
        }
        case (InferSolveSuccess(i)) => (i)
      }

    addInnerDataToNearEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    val prototype =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
        innerEnv, temputs, functionA)
    (EvaluateFunctionSuccess(prototype))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForBanner(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionA: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    val inferredTemplatas =
      InferTemplar.inferFromArgCoords(
        nearEnv.snapshot,
        temputs,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        alreadySpecifiedTemplateArgs,
        paramFilters) match {
      case (isf @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (EvaluateFunctionFailure(isf.toString))
      }
      case (InferSolveSuccess(i)) => (i)
    }

    addInnerDataToNearEnv(nearEnv, functionA.identifyingRunes, inferredTemplatas.templatasByRune, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    val banner =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        innerEnv, temputs, functionA)
    (EvaluateFunctionSuccess(banner))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateTemplatedFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionA: FunctionA):
  (FunctionHeader2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(functionA.isTemplate)

    vassert(functionA.identifyingRunes.size == List().size);

    val result =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv.snapshot,
        temputs,
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

    addInnerDataToNearEnv(nearEnv, List(), inferences.templatasByRune, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(innerEnv, temputs, functionA)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateOrdinaryFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionA: FunctionA):
  (FunctionHeader2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv.snapshot, temputs, functionA.templateRules, functionA.typeByRune)
    addInnerDataToNearEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForHeader(
      innerEnv, temputs, functionA)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryFunctionFromNonCallForPrototype(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    functionA: FunctionA):
  (Prototype2) = {
    // Check preconditions

    checkClosureConcernsHandled(nearEnv, functionA)
    vassert(!functionA.isTemplate)

    val inferences =
      InferTemplar.inferOrdinaryRules(
        nearEnv.snapshot, temputs, functionA.templateRules, functionA.typeByRune)
    addInnerDataToNearEnv(nearEnv, List(), inferences, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    FunctionTemplarMiddleLayer.getOrEvaluateFunctionForPrototype(
      innerEnv, temputs, functionA)
  }


  // This is called while we're trying to figure out what functionSs to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      // The environment the function was defined in.
      nearEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionA: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    // Check preconditions
    functionA.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(functionA.isTemplate)


    val inferences =
      InferTemplar.inferFromArgCoords(
        nearEnv.snapshot,
        temputs,
        functionA.identifyingRunes,
        functionA.templateRules,
        functionA.typeByRune,
        functionA.params.map(_.pattern),
        functionA.maybeRetCoordRune,
        explicitTemplateArgs,
        args) match {
      case (isc @ InferSolveFailure(_, _, _, _, _, _)) => {
        return (EvaluateFunctionFailure[FunctionBanner2](isc.toString))
      }
      case (InferSolveSuccess(inferredTemplatas)) => (inferredTemplatas.templatasByRune)
    }

    // See FunctionTemplar doc for what outer/runes/inner envs are.
    addInnerDataToNearEnv(nearEnv, functionA.identifyingRunes, inferences, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    val banner =
      FunctionTemplarMiddleLayer.getOrEvaluateFunctionForBanner(
        innerEnv, temputs, functionA)

    (EvaluateFunctionSuccess(banner))
  }

  def scanOrdinaryInterfaceMember(
    nearEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    interfaceExplicitTemplateArgs: List[ITemplata],
    functionA: FunctionA):
  (FunctionHeader2) = {
    val result =
      InferTemplar.inferFromExplicitTemplateArgs(
        nearEnv.snapshot,
        temputs,
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

    addInnerDataToNearEnv(nearEnv, functionA.identifyingRunes, templataByRune, functionA.maybeRetCoordRune)
    val innerEnv = nearEnv

    val params2 =
      FunctionTemplarMiddleLayer.assembleFunctionParams(
        innerEnv.snapshot, temputs, functionA.params)

    val Some(CoordTemplata(retCoord)) =
      innerEnv.getNearestTemplataWithName(functionA.maybeRetCoordRune.get, Set(TemplataLookupContext))


    temputs.declareFunctionSignature(
      Signature2(innerEnv.fullName, params2.map(_.tyype)),
      Some(innerEnv.snapshot))
    val header =
      FunctionTemplarCore.makeInterfaceFunction(
        innerEnv, temputs, Some(functionA), params2, retCoord)
    header
  }

  private def checkClosureConcernsHandled(
    // The environment the function was defined in.
    nearEnv: FunctionEnvironmentBox,
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

  // IOW, add the necessary data to turn the near env into the inner env.
  def addInnerDataToNearEnv(
    nearEnv: FunctionEnvironmentBox,
    identifyingRunes: List[String],
    templatasByRune: Map[String, ITemplata],
    maybeRetCoordRune: Option[String]) = {

    val identifyingTemplatas = identifyingRunes.map(templatasByRune)
    val fullName = FullName2(nearEnv.fullName.steps.init :+ nearEnv.fullName.steps.last.copy(templateArgs = Some(identifyingTemplatas)))
    val maybeReturnType = maybeRetCoordRune.map(retCoordRune => {
      templatasByRune.get(retCoordRune) match {
        case Some(CoordTemplata(coord)) => coord
        case _ => vwat()
      }
    })
    // Change the fullName's templateArgs from None to Some(List()) to be consistent with
    // the rest of the layer.
    nearEnv.setFullName(fullName)
    nearEnv.setReturnType(maybeReturnType)
    nearEnv.addEntries(templatasByRune.mapValues(x => List(TemplataEnvEntry(x))))
  }
}
