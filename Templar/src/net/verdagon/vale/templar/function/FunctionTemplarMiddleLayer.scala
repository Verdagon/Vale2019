package net.verdagon.vale.templar.function

import net.verdagon.vale.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, OverrideSP, VirtualitySP}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env._
import net.verdagon.vale.{vassert, vcurious, vfail}

import scala.collection.immutable.{List, Set}

object FunctionTemplarMiddleLayer {

  // This is for the early stages of Templar when it's scanning banners to put in
  // its env. We just want its banner, we don't want to evaluate it.
  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def predictOrdinaryFunctionBanner(
    runedEnv: FunctionEnvironment,
    temputs: TemputsBox,
    function1: FunctionA):
  (FunctionBanner2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(runedEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty)
    })
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    val params2 = assembleFunctionParams(runedEnv, temputs, function1.params)
    val namedEnv = makeNamedEnv(runedEnv, params2.map(_.tyype))
    val banner = FunctionBanner2(Some(function1), namedEnv.fullName, params2)
    banner
  }

  private def evaluateMaybeVirtuality(
      env: IEnvironment,
      temputs: TemputsBox,
      maybeVirtuality1: Option[VirtualitySP]):
  (Option[Virtuality2]) = {
    maybeVirtuality1 match {
      case None => (None)
      case Some(AbstractSP) => (Some(Abstract2))
      case Some(OverrideSP(interfaceRune)) => {
        env.getNearestTemplataWithName(interfaceRune, Set(TemplataLookupContext)) match {
          case None => vcurious()
          case Some(KindTemplata(ir @ InterfaceRef2(_))) => (Some(Override2(ir)))
          case Some(it @ InterfaceTemplata(_, _)) => {
            val ir =
              StructTemplar.getInterfaceRef(temputs, it, List())
            (Some(Override2(ir)))
          }
        }
      }
    }
  }

  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def getOrEvaluateFunctionForBanner(
    runedEnv: FunctionEnvironment,
    temputs: TemputsBox,
    function1: FunctionA):
  (FunctionBanner2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(runedEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val params2 = assembleFunctionParams(runedEnv, temputs, function1.params)

    val namedEnv = makeNamedEnv(runedEnv, params2.map(_.tyype))
    val banner = FunctionBanner2(Some(function1), namedEnv.fullName, params2)

    // Now we want to add its Function2 into the temputs.
    if (temputs.exactDeclaredSignatureExists(banner.toSignature)) {
      // Someone else is already working on it (or has finished), so
      // just return.
      (banner)
    } else {
      val signature = banner.toSignature
      temputs.declareFunctionSignature(signature, Some(namedEnv))
      val params2 = assembleFunctionParams(namedEnv, temputs, function1.params)
      val header =
        FunctionTemplarCore.evaluateFunctionForHeader(namedEnv, temputs, function1, params2)
      if (header.toBanner != banner) {
        val bannerFromHeader = header.toBanner
        vfail("wut\n" + bannerFromHeader + "\n" + banner)
      }

      VirtualTemplar.evaluateParent(namedEnv, temputs, header)

      (header.toBanner)
    }
  }

  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def getOrEvaluateFunctionForHeader(
    runedEnv: FunctionEnvironment,
    temputs: TemputsBox,
    function1: FunctionA):
  (FunctionHeader2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(runedEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val paramTypes2 = evaluateFunctionParamTypes(runedEnv, function1.params);
    val functionFullName = makeFunctionFullName(runedEnv.fullName, paramTypes2)
    val needleSignature = Signature2(functionFullName, paramTypes2)
    temputs.lookupFunction(needleSignature) match {
      case Some(Function2(header, _, _)) => {
        (header)
      }
      case None => {
        val params2 = assembleFunctionParams(runedEnv, temputs, function1.params)

        val namedEnv = makeNamedEnv(runedEnv, params2.map(_.tyype))

        temputs.declareFunctionSignature(needleSignature, Some(namedEnv))

        val header =
          FunctionTemplarCore.evaluateFunctionForHeader(
            namedEnv, temputs, function1, params2)
        vassert(header.toSignature == needleSignature)
        (header)
      }
    }
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // fn main():Int{main()}
  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def getOrEvaluateFunctionForPrototype(
    runedEnv: FunctionEnvironment,
    temputs: TemputsBox,
    function1: FunctionA):
  (Prototype2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(runedEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val paramTypes2 = evaluateFunctionParamTypes(runedEnv, function1.params)
    val namedEnv = makeNamedEnv(runedEnv, paramTypes2)
    val needleSignature = Signature2(namedEnv.fullName, paramTypes2)
    temputs.returnTypesBySignature.get(needleSignature) match {
      case Some(returnType2) => {
        (Prototype2(namedEnv.fullName, paramTypes2, returnType2))
      }
      case None => {
        if (temputs.exactDeclaredSignatureExists(needleSignature)) {
          vfail("Need return type for " + needleSignature + ", cycle found")
        }
        temputs.declareFunctionSignature(needleSignature, Some(namedEnv))
        val params2 = assembleFunctionParams(namedEnv, temputs, function1.params)
        val header =
          FunctionTemplarCore.evaluateFunctionForHeader(
            namedEnv, temputs, function1, params2)

        VirtualTemplar.evaluateParent(namedEnv, temputs, header)

        vassert(header.toSignature == needleSignature)
        (header.toPrototype)
      }
    }
  }



  private def evaluateFunctionParamTypes(
    env: IEnvironment,
    params1: List[ParameterS]):
  List[Coord] = {
    params1.map(param1 => {
      val CoordTemplata(coord) = env.getNearestTemplataWithName(param1.pattern.coordRune, Set(TemplataLookupContext)).get
      coord
    })
  }

  def assembleFunctionParams(
    env: IEnvironment,
    temputs: TemputsBox,
    params1: List[ParameterS]):
  (List[Parameter2]) = {
    params1.foldLeft((List[Parameter2]()))({
      case ((previousParams2), param1) => {
        val CoordTemplata(coord) = env.getNearestTemplataWithName(param1.pattern.coordRune, Set(TemplataLookupContext)).get
        val maybeVirtuality =
          evaluateMaybeVirtuality(env, temputs, param1.pattern.virtuality)
        (previousParams2 :+ Parameter2(param1.name.name, maybeVirtuality, coord))
      }
    })
  }

//  def makeImplDestructor(
//    env: IEnvironment,
//    temputs: TemputsBox,
//    structDef2: StructDefinition2,
//    interfaceRef2: InterfaceRef2):
//  Temputs = {
//    val ownership = if (structDef2.mutability == MutableP) Own else Share
//    val structRef2 = structDef2.getRef
//    val structType2 = Coord(ownership, structRef2)
//    val interfaceType2 = Coord(ownership, interfaceRef2)
//    val signature2 =
//      Signature2(
//        CallTemplar.INTERFACE_DESTRUCTOR_NAME,
//        List(CoercedFinalTemplateArg2(ReferenceTemplata(interfaceType2))),
//        List(structType2))
//    temputs.declareFunctionSignature(signature2)
//
//    val header =
//      FunctionTemplarCore.makeImplDestructor(
//        env, temputs, structDef2, interfaceRef2)
//
//
//      VirtualTemplar.evaluateParent(env, temputs, header)
//
//
//      VirtualTemplar.evaluateOverrides(env, temputs, header)
//
//    temputs
//  }

  def makeNamedEnv(runedEnv: FunctionEnvironment, paramTypes: List[Coord]):
  FunctionEnvironment = {
    // The last step is the name, but it doesn't have the params filled out.
    // (these asserts are just to make sure that's still the case)
    vassert(runedEnv.fullName.last.parameters.isEmpty)
    // We fill out the params here to get the function's full name.
    val functionFullName = makeFunctionFullName(runedEnv.fullName, paramTypes)
    val namedEnv = runedEnv.copy(fullName = functionFullName)
    namedEnv
  }

  def makeFunctionFullName(runedEnvFullName: FullName2, paramTypes: List[Coord]): FullName2 = {
    vassert(runedEnvFullName.last.parameters.isEmpty)
    // We fill out the params here to get the function's full name.
    val functionFullName =
      FullName2(
        runedEnvFullName.steps.init :+
          runedEnvFullName.last.copy(parameters = Some(paramTypes)))
    functionFullName
  }
}
