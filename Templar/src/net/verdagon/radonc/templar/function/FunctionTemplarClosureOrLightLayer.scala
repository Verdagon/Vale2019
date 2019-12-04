package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout.CodeBody1
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.radonc.vassert

import scala.collection.immutable.List

// When templaring a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
object FunctionTemplarClosureOrLightLayer {

  // This is for the early stages of Templar when it's scanning banners to put in
  // its env. We just want its banner, we don't want to evaluate it.
  def predictOrdinaryLightFunctionBanner(
    outerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function1.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.predictOrdinaryFunctionBanner(
      outerEnv,
      temputs0,
      function1)
  }


  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function1.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      outerEnv,
      temputs0,
      function1)
  }

  def evaluateTemplatedClosureFunctionFromCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    val containingFunctionLambdaNumber = functionS.lambdaNumber

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv.addVariables(
        closureStructDef.members.map(member => {
          member.tyype match {
            case AddressMemberType2(reference) => {
              AddressibleClosureVariable2(
                VariableId2(containingFunctionLambdaNumber, member.name),
                closureStructRef,
                member.variability,
                reference)
            }
            case ReferenceMemberType2(reference) => {
              ReferenceClosureVariable2(
                VariableId2(containingFunctionLambdaNumber, member.name),
                closureStructRef,
                member.variability,
                reference)
            }
          }
        }))
        .addEntry(closureStructRef.fullName.steps.last.humanName, TemplataEnvEntry(KindTemplata(closureStructRef)))

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
      nearEnv, temputs0, functionS, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    outerEnv: FunctionEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    val containingFunctionLambdaNumber = functionS.lambdaNumber

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv.addVariables(
        closureStructDef.members.map(member => {
          member.tyype match {
            case AddressMemberType2(reference) => {
              AddressibleClosureVariable2(
                VariableId2(containingFunctionLambdaNumber, member.name),
                closureStructRef,
                member.variability,
                reference)
            }
            case ReferenceMemberType2(reference) => {
              ReferenceClosureVariable2(
                VariableId2(containingFunctionLambdaNumber, member.name),
                closureStructRef,
                member.variability,
                reference)
            }
          }
        }))
        .addEntry(closureStructRef.fullName.steps.last.humanName, TemplataEnvEntry(KindTemplata(closureStructRef)))

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      nearEnv, temputs0, functionS, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromNonCallForHeader(
      ourEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata]):
  (Temputs, FunctionHeader2) = {
    vassert(functionS.identifyingRunes.size == explicitTemplateArgs.size);
    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromNonCallForHeader(
      ourEnv, temputs0, functionS)
  }

  def evaluateTemplatedLightFunctionFromCallForPrototype2(
      ourEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[Prototype2]) = {
    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      ourEnv, temputs0, functionS, explicitTemplateArgs, args)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironment,
      temputs0: Temputs,
      function1: FunctionA):
  (Temputs, FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      outerEnv, temputs0, function1)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    outerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, Prototype2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForPrototype(
      outerEnv, temputs0, function1)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    outerEnv: FunctionEnvironment,
    temputs0: Temputs,
    closureStructRef: StructRef2,
    function1: FunctionA):
  (Temputs, FunctionBanner2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv.addVariables(
        closureStructDef.members.map(member => {
          val containingFunctionLambdaNumber = outerEnv.function.lambdaNumber
          val variableId = VariableId2(containingFunctionLambdaNumber, member.name)
          member.tyype match {
            case AddressMemberType2(reference) => {
              AddressibleClosureVariable2(variableId, closureStructRef, member.variability, reference)
            }
            case ReferenceMemberType2(reference) => {
              ReferenceClosureVariable2(variableId, closureStructRef, member.variability, reference)
            }
          }
        }))
      .addEntry(
        closureStructRef.fullName.steps.last.humanName,
        TemplataEnvEntry(KindTemplata(closureStructRef)))

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      nearEnv, temputs0, function1)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      function1: FunctionA):
  (Temputs, FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv.addVariables(
        closureStructDef.members.map(member => {
          val containingFunctionLambdaNumber = outerEnv.function.lambdaNumber
          val variableId = VariableId2(containingFunctionLambdaNumber, member.name)
          member.tyype match {
            case AddressMemberType2(reference) => {
              AddressibleClosureVariable2(variableId, closureStructRef, member.variability, reference)
            }
            case ReferenceMemberType2(reference) => {
              ReferenceClosureVariable2(variableId, closureStructRef, member.variability, reference)
            }
          }
        }))
      .addEntry(
        closureStructRef.fullName.steps.last.humanName,
        TemplataEnvEntry(KindTemplata(closureStructRef)))

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      nearEnv, temputs0, function1)
  }

  // This is called while we're trying to figure out what function1s to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      functionOuterEnv: FunctionEnvironment,
      temputs0: Temputs,
      function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedLightBannerFromCall(
        functionOuterEnv, temputs0, function, explicitTemplateArgs, args)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs0: Temputs,
      functionS: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (Temputs, IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
        outerEnv, temputs0, functionS,
        alreadySpecifiedTemplateArgs,
        paramFilters)
  }

  def scanOrdinaryInterfaceMember(
    env1: FunctionEnvironment,
    temputs0: Temputs,
    interfaceExplicitTemplateArgs: List[ITemplata],
    prototype1: FunctionA):
  (Temputs, FunctionHeader2) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.scanOrdinaryInterfaceMember(env1, temputs0, interfaceExplicitTemplateArgs, prototype1)
  }
}
