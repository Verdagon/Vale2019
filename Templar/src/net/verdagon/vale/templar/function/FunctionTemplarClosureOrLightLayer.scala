package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.CodeBody1
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.vale.vassert

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
    outerEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    function1: FunctionA):
  (FunctionBanner2) = {
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function1.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.predictOrdinaryFunctionBanner(
      outerEnv,
      temputs,
      function1)
  }


  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      outerEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      function1: FunctionA):
  (FunctionBanner2) = {
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function1.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      outerEnv,
      temputs,
      function1)
  }

  def evaluateTemplatedClosureFunctionFromCallForBanner(
      outerEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
      functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val containingFunctionLambdaNumber = functionS.lambdaNumber

    val closureStructDef = temputs.lookupStruct(closureStructRef);

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
    outerEnv.addEntry(closureStructRef.fullName.steps.last.humanName, TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.
    val nearEnv = outerEnv

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
      nearEnv, temputs, functionS, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    outerEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    functionS: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    val containingFunctionLambdaNumber = functionS.lambdaNumber

    val closureStructDef = temputs.lookupStruct(closureStructRef);

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
    outerEnv.addEntry(closureStructRef.fullName.steps.last.humanName, TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.
    val nearEnv = outerEnv

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      nearEnv, temputs, functionS, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromNonCallForHeader(
      ourEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata]):
  (FunctionHeader2) = {
    vassert(functionS.identifyingRunes.size == explicitTemplateArgs.size);
    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromNonCallForHeader(
      ourEnv, temputs, functionS)
  }

  def evaluateTemplatedLightFunctionFromCallForPrototype2(
      ourEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionS: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    functionS.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      ourEnv, temputs, functionS, explicitTemplateArgs, args)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      function1: FunctionA):
  (FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      outerEnv, temputs, function1)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    outerEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    function1: FunctionA):
  (Prototype2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForPrototype(
      outerEnv, temputs, function1)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    outerEnv: FunctionEnvironmentBox,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function1: FunctionA):
  (FunctionBanner2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    val closureStructDef = temputs.lookupStruct(closureStructRef);

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
    outerEnv.addEntry(
      closureStructRef.fullName.steps.last.humanName,
      TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.
    val nearEnv = outerEnv

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      nearEnv, temputs, function1)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
      function1: FunctionA):
  (FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function1.isTemplate)

    val closureStructDef = temputs.lookupStruct(closureStructRef);

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
    outerEnv.addEntry(
      closureStructRef.fullName.steps.last.humanName,
      TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.
    val nearEnv = outerEnv

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      nearEnv, temputs, function1)
  }

  // This is called while we're trying to figure out what function1s to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      functionOuterEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedLightBannerFromCall(
        functionOuterEnv, temputs, function, explicitTemplateArgs, args)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
      outerEnv: FunctionEnvironmentBox,
      temputs: TemputsBox,
      functionS: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
        outerEnv, temputs, functionS,
        alreadySpecifiedTemplateArgs,
        paramFilters)
  }

  def scanOrdinaryInterfaceMember(
    env1: FunctionEnvironmentBox,
    temputs: TemputsBox,
    interfaceExplicitTemplateArgs: List[ITemplata],
    prototype1: FunctionA):
  (FunctionHeader2) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.scanOrdinaryInterfaceMember(env1, temputs, interfaceExplicitTemplateArgs, prototype1)
  }
}