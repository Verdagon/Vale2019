package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.CodeBody1
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.FunctionTemplar.IEvaluateFunctionResult
import net.verdagon.vale.{vassert, vimpl}

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
    temputs: TemputsBox,
    function: FunctionA):
  (FunctionBanner2) = {
    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.predictOrdinaryFunctionBanner(
      outerEnv,
      temputs,
      function)
  }


  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA):
  (FunctionBanner2) = {
    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(!function.isTemplate)
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      outerEnv,
      temputs,
      function)
  }

  def evaluateTemplatedClosureFunctionFromCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
    function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    val containingFunctionLambdaNumber = vimpl()//function.lambdaNumber

    val closureStructDef = temputs.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv
        .addVariables(
          closureStructDef.members.map(member => {
            member.tyype match {
              case AddressMemberType2(reference) => {
                AddressibleClosureVariable2(
                  FullName2(containingFunctionLambdaNumber, member.name),
                  closureStructRef,
                  member.variability,
                  reference)
              }
              case ReferenceMemberType2(reference) => {
                ReferenceClosureVariable2(
                  FullName2(containingFunctionLambdaNumber, member.name),
                  closureStructRef,
                  member.variability,
                  reference)
              }
            }
          }))
        .addEntry(closureStructRef.fullName.last, TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
      nearEnv, temputs, function, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    outerEnv: FunctionEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function: FunctionA,
    alreadySpecifiedTemplateArgs: List[ITemplata],
    argTypes2: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    val containingFunctionLambdaNumber = vimpl()//function.lambdaNumber

    val closureStructDef = temputs.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv
        .addVariables(
          closureStructDef.members.map(member => {
            member.tyype match {
              case AddressMemberType2(reference) => {
                AddressibleClosureVariable2(
                  FullName2(containingFunctionLambdaNumber, member.name),
                  closureStructRef,
                  member.variability,
                  reference)
              }
              case ReferenceMemberType2(reference) => {
                ReferenceClosureVariable2(
                  FullName2(containingFunctionLambdaNumber, member.name),
                  closureStructRef,
                  member.variability,
                  reference)
              }
            }
          }))
        .addEntry(closureStructRef.fullName.last, TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      nearEnv, temputs, function, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromNonCallForHeader(
      ourEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA,
      explicitTemplateArgs: List[ITemplata]):
  (FunctionHeader2) = {
    vassert(function.identifyingRunes.size == explicitTemplateArgs.size);
    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromNonCallForHeader(
      ourEnv, temputs, function)
  }

  def evaluateTemplatedLightFunctionFromCallForPrototype2(
      ourEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[Prototype2]) = {
    function.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      ourEnv, temputs, function, explicitTemplateArgs, args)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA):
  (FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      outerEnv, temputs, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
    outerEnv: FunctionEnvironment,
    temputs: TemputsBox,
    function: FunctionA
  ): Prototype2 = {
    // This should only be called with a non-templated function
    vassert(!function.isTemplate)

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForPrototype(
      outerEnv, temputs, function)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    outerEnv: FunctionEnvironment,
    temputs: TemputsBox,
    closureStructRef: StructRef2,
    function: FunctionA):
  (FunctionBanner2) = {
    // This should only be called with a non-templated function
    vassert(!function.isTemplate)

    val closureStructDef = temputs.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv
        .addVariables(
        closureStructDef.members.map(member => {
          val containingFunctionLambdaNumber = vimpl()//outerEnv.function.lambdaNumber
          val variableId = FullName2(containingFunctionLambdaNumber, member.name)
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
        closureStructRef.fullName.last,
        TemplataEnvEntry(KindTemplata(closureStructRef)))
      // Now that the variables are added, we've modified the outerEnv to be the nearEnv.

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      nearEnv, temputs, function)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      outerEnv: FunctionEnvironment,
      temputs: TemputsBox,
      closureStructRef: StructRef2,
    function: FunctionA):
  (FunctionHeader2) = {
    // This should only be called with a non-templated function
    vassert(!function.isTemplate)

    val closureStructDef = temputs.lookupStruct(closureStructRef);

    val nearEnv =
      outerEnv
        .addVariables(
          closureStructDef.members.map(member => {
            val containingFunctionLambdaNumber = vimpl()//outerEnv.function.lambdaNumber
            val variableId = FullName2(containingFunctionLambdaNumber, member.name)
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
          closureStructRef.fullName.last,
          TemplataEnvEntry(KindTemplata(closureStructRef)))
    // Now that the variables are added, we've modified the outerEnv to be the nearEnv.

    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      nearEnv, temputs, function)
  }

  // This is called while we're trying to figure out what function1s to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      functionOuterEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA,
      explicitTemplateArgs: List[ITemplata],
      args: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedLightBannerFromCall(
        functionOuterEnv, temputs, function, explicitTemplateArgs, args)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
      outerEnv: FunctionEnvironment,
      temputs: TemputsBox,
    function: FunctionA,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      paramFilters: List[ParamFilter]):
  (IEvaluateFunctionResult[FunctionBanner2]) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
        outerEnv, temputs, function,
        alreadySpecifiedTemplateArgs,
        paramFilters)
  }

  def scanOrdinaryInterfaceMember(
    env1: FunctionEnvironment,
    temputs: TemputsBox,
    interfaceExplicitTemplateArgs: List[ITemplata],
    function: FunctionA):
  (FunctionHeader2) = {
    FunctionTemplarOrdinaryOrTemplatedLayer.scanOrdinaryInterfaceMember(
      env1, temputs, interfaceExplicitTemplateArgs, function)
  }
}
