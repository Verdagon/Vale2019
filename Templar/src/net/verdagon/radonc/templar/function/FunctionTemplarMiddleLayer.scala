package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.{CodeBodyA, FunctionA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, OverrideSP, VirtualitySP}
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env.{ExpressionLookupContext, FunctionEnvironment, IEnvironment, TemplataLookupContext}
import net.verdagon.radonc.{vassert, vcurious, vfail}

import scala.collection.immutable.{List, Set}

object FunctionTemplarMiddleLayer {

  // This is for the early stages of Templar when it's scanning banners to put in
  // its env. We just want its banner, we don't want to evaluate it.
  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def predictOrdinaryFunctionBanner(
    innerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, FunctionBanner2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(innerEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty)
    })
    function1.body match {
      case CodeBodyA(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    val (temputs1, params2) = assembleFunctionParams(innerEnv, temputs0, function1.params)
    val banner = FunctionBanner2(Some(function1), innerEnv.fullName, params2)
    (temputs1, banner)
  }

  private def evaluateMaybeVirtuality(
      env: IEnvironment,
      temputs0: Temputs,
      maybeVirtuality1: Option[VirtualitySP]):
  (Temputs, Option[Virtuality2]) = {
    maybeVirtuality1 match {
      case None => (temputs0, None)
      case Some(AbstractSP) => (temputs0, Some(Abstract2))
      case Some(OverrideSP(interfaceRune)) => {
        env.getNearestTemplataWithName(interfaceRune, Set(TemplataLookupContext)) match {
          case None => vcurious()
          case Some(KindTemplata(ir @ InterfaceRef2(_))) => (temputs0, Some(Override2(ir)))
          case Some(it @ InterfaceTemplata(_, _)) => {
            val (temputs1, ir) =
              StructTemplar.getInterfaceRef(temputs0, it, List())
            (temputs1, Some(Override2(ir)))
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
    innerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, FunctionBanner2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(innerEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val (temputs1, params2) = assembleFunctionParams(innerEnv, temputs0, function1.params)
    val banner = FunctionBanner2(Some(function1), innerEnv.fullName, params2)

    // Now we want to add its Function2 into the temputs.
    if (temputs0.exactDeclaredSignatureExists(banner.toSignature)) {
      // Someone else is already working on it (or has finished), so
      // just return.
      (temputs1, banner)
    } else {
      val signature = banner.toSignature
      val temputs2 = temputs1.declareFunctionSignature(signature, Some(innerEnv))
      val (temputs3, params2) = assembleFunctionParams(innerEnv, temputs2, function1.params)
      val (temputs4, header) =
        FunctionTemplarCore.evaluateFunctionForHeader(innerEnv, temputs3, function1, params2)
      if (header.toBanner != banner) {
        val bannerFromHeader = header.toBanner
        vfail("wut\n" + bannerFromHeader + "\n" + banner)
      }

      val temputs5 =
        VirtualTemplar.evaluateParent(innerEnv, temputs4, header)

      val temputs6 =
        VirtualTemplar.evaluateOverrides(innerEnv, temputs5, header)

      (temputs6, header.toBanner)
    }
  }

  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def getOrEvaluateFunctionForHeader(
    innerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, FunctionHeader2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(innerEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val paramTypes2 = evaluateFunctionParamTypes(innerEnv, function1.params);
    val needleSignature = Signature2(innerEnv.fullName, paramTypes2)
    temputs0.lookupFunction(needleSignature) match {
      case Some(Function2(header, _, _)) => {
        (temputs0, header)
      }
      case None => {
        val temputs1 = temputs0.declareFunctionSignature(needleSignature, Some(innerEnv))
        val (temputs2, params2) = assembleFunctionParams(innerEnv, temputs1, function1.params)
        val (temputs3, header) =
          FunctionTemplarCore.evaluateFunctionForHeader(
            innerEnv, temputs2, function1, params2)
        vassert(header.toSignature == needleSignature)
        (temputs3, header)
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
    innerEnv: FunctionEnvironment,
    temputs0: Temputs,
    function1: FunctionA):
  (Temputs, Prototype2) = {

    // Check preconditions
    function1.typeByRune.keySet.foreach(templateParam => {
      vassert(innerEnv.getNearestTemplataWithName(templateParam, Set(TemplataLookupContext, ExpressionLookupContext)).nonEmpty);
    })

    val paramTypes2 = evaluateFunctionParamTypes(innerEnv, function1.params)
    val needleSignature = Signature2(innerEnv.fullName, paramTypes2)
    temputs0.returnTypesBySignature.get(needleSignature) match {
      case Some(returnType2) => {
        (temputs0, Prototype2(innerEnv.fullName, FunctionT2(paramTypes2, returnType2)))
      }
      case None => {
        if (temputs0.exactDeclaredSignatureExists(needleSignature)) {
          vfail("Need return type for " + needleSignature + ", cycle found")
        }
        val temputs1 = temputs0.declareFunctionSignature(needleSignature, Some(innerEnv))
        val (temputs2, params2) = assembleFunctionParams(innerEnv, temputs1, function1.params)
        val (temputs3, header) =
          FunctionTemplarCore.evaluateFunctionForHeader(
            innerEnv, temputs2, function1, params2)

        val temputs5 =
          VirtualTemplar.evaluateParent(innerEnv, temputs3, header)

        val temputs6 =
          VirtualTemplar.evaluateOverrides(innerEnv, temputs5, header)

        vassert(header.toSignature == needleSignature)
        (temputs6, header.toPrototype)
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
    temputs0: Temputs,
    params1: List[ParameterS]):
  (Temputs, List[Parameter2]) = {
    params1.foldLeft((temputs0, List[Parameter2]()))({
      case ((temputs1, previousParams2), param1) => {
        val CoordTemplata(coord) = env.getNearestTemplataWithName(param1.pattern.coordRune, Set(TemplataLookupContext)).get
        val (temputs2, maybeVirtuality) =
          evaluateMaybeVirtuality(env, temputs1, param1.pattern.virtuality)
        (temputs2, previousParams2 :+ Parameter2(param1.name.name, maybeVirtuality, coord))
      }
    })
  }

//  def makeImplDestructor(
//    env: IEnvironment,
//    temputs0: Temputs,
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
//    val temputs1 = temputs0.declareFunctionSignature(signature2)
//
//    val (temputs3, header) =
//      FunctionTemplarCore.makeImplDestructor(
//        env, temputs1, structDef2, interfaceRef2)
//
//    val temputs5 =
//      VirtualTemplar.evaluateParent(env, temputs3, header)
//
//    val temputs6 =
//      VirtualTemplar.evaluateOverrides(env, temputs5, header)
//
//    temputs6
//  }
}
