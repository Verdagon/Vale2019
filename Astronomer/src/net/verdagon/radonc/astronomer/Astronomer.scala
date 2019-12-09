package net.verdagon.radonc.astronomer

import net.verdagon.radonc.astronomer.builtins._
import net.verdagon.radonc.astronomer.externs.Externs
import net.verdagon.radonc.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.radonc.parser.{ImmutableP, MutabilityP, MutableP}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.{vassert, vfail, vimpl}

import scala.collection.immutable.List

case class Environment(
    maybeParentEnv: Option[Environment],
    primitives: Map[String, ITypeSR],
    structs: List[StructS],
    interfaces: List[InterfaceS],
    impls: List[ImplS],
    functions: List[FunctionS],
    typeByRune: Map[String, ITemplataType]) {
  def addRunes(newTypeByRune: Map[String, ITemplataType]): Environment = {
    Environment(maybeParentEnv, primitives, structs, interfaces, impls, functions, typeByRune ++ newTypeByRune)
  }

  def lookup(name: String):
  (Option[ITypeSR], List[StructS], List[InterfaceS], Option[ITemplataType]) = {
    val nearPrimitives = primitives.get(name)
    val nearStructs = structs.filter(_.name == name)
    val nearInterfaces = interfaces.filter(_.name == name)
    val nearRuneType = typeByRune.get(name)

    if (nearPrimitives.nonEmpty || nearStructs.nonEmpty || nearInterfaces.nonEmpty || nearRuneType.nonEmpty) {
      return (nearPrimitives, nearStructs, nearInterfaces, nearRuneType)
    }
    maybeParentEnv match {
      case None => (None, List(), List(), None)
      case Some(parentEnv) => parentEnv.lookup(name)
    }
  }
}

case class AstroutsBox(var astrouts: Astrouts) {
  def getImpl(codeLocation: CodeLocation) = {
    astrouts.impls.get(codeLocation)
  }
  def getStruct(codeLocation: CodeLocation) = {
    astrouts.structs.get(codeLocation)
  }
  def getInterface(codeLocation: CodeLocation) = {
    astrouts.interfaces.get(codeLocation)
  }
}

case class Astrouts(
  structs: Map[CodeLocation, StructA],
  interfaces: Map[CodeLocation, InterfaceA],
  impls: Map[CodeLocation, ImplA],
  functions: Map[CodeLocation, FunctionA])

object Astronomer {
  val primitives =
    Map(
      "Int" -> KindTypeSR,
      "Str" -> KindTypeSR,
      "Bool" -> KindTypeSR,
      "Float" -> KindTypeSR,
      "Void" -> KindTypeSR,
      "IFunction1" -> TemplateTypeSR(List(MutabilityTypeSR, CoordTypeSR, CoordTypeSR), KindTypeSR),
      "__Array" -> TemplateTypeSR(List(MutabilityTypeSR, CoordTypeSR), KindTypeSR))

  def translateStructs(astrouts: AstroutsBox, env: Environment, structsS: List[StructS]): List[StructA] = {
    structsS match {
      case Nil => Nil
      case headS :: tailS => translateStruct(astrouts, env, headS) :: translateStructs(astrouts, env, tailS)
    }
  }

  def translateRuneType(tyype: ITypeSR): ITemplataType = {
    tyype match {
      case IntTypeSR => IntegerTemplataType
      case BoolTypeSR => BooleanTemplataType
      case OwnershipTypeSR => OwnershipTemplataType
      case MutabilityTypeSR => MutabilityTemplataType
      case PermissionTypeSR => PermissionTemplataType
      case LocationTypeSR => LocationTemplataType
      case CoordTypeSR => CoordTemplataType
      case KindTypeSR => KindTemplataType
      case FunctionTypeSR => FunctionTemplataType
      case TemplateTypeSR(params, result) => TemplateTemplataType(params.map(translateRuneType), translateRuneType(result))
      case VariabilityTypeSR => VariabilityTemplataType
    }
  }

  def lookupStructType(astrouts: AstroutsBox, env: Environment, structS: StructS): ITemplataType = {
    structS.maybePredictedType match {
      case Some(predictedType) => {
        translateRuneType(predictedType)
      }
      case None => {
        val structA = translateStruct(astrouts, env, structS)
        structA.tyype
      }
    }
  }

  def lookupInterfaceType(astrouts: AstroutsBox, env: Environment, interfaceS: InterfaceS):
  ITemplataType = {
    interfaceS.maybePredictedType match {
      case Some(predictedType) => {
        translateRuneType(predictedType)
      }
      case None => {
        val interfaceA = translateInterface(astrouts, env, interfaceS)
        interfaceA.tyype
      }
    }
  }

  def lookupType(astrouts: AstroutsBox, env: Environment, name: String): ITemplataType = {
    // When the scout comes across a lambda, it doesn't put the e.g. __Closure:main:lam1 struct into
    // the environment or anything, it lets templar to do that (because templar knows the actual types).
    // However, this means that when the lambda function gets to the astronomer, the astronomer doesn't
    // know what to do with it.
    // TODO: Make it so names aren't strings, but instead sealed traits and case classes.
    // Or, figure out something else to avoid this oddity here.
    if (name.startsWith(FunctionScout.CLOSURE_STRUCT_NAME)) {
      return KindTemplataType
    }

    val (primitivesS, structsS, interfacesS, maybeRuneTypeS) = env.lookup(name)

    if (primitivesS.isEmpty && structsS.isEmpty && interfacesS.isEmpty && maybeRuneTypeS.isEmpty) {
      vfail("Nothing found with name " + name)
    }
    if (primitivesS.size.signum + structsS.size.signum + interfacesS.size.signum > 1) {
      vfail("Name doesn't correspond to only one of primitive or struct or interface: " + name)
    }

    if (primitivesS.nonEmpty) {
      vassert(primitivesS.size == 1)
      translateRuneType(primitivesS.get)
    } else if (maybeRuneTypeS.nonEmpty) {
      maybeRuneTypeS.get
    } else if (structsS.nonEmpty) {
      val types = structsS.map(lookupStructType(astrouts, env, _))
      if (types.toSet.size > 1) {
        vfail("'" + name + "' has multiple types: " + types.toSet)
      }
      val tyype = types.head
      tyype
    } else if (interfacesS.nonEmpty) {
      val types = interfacesS.map(lookupInterfaceType(astrouts, env, _))
      if (types.toSet.size > 1) {
        vfail("'" + name + "' has multiple types: " + types.toSet)
      }
      val tyype = types.head
      tyype
    } else vfail()
  }

  def makeRuleTyper(): RuleTyperEvaluator[Environment, AstroutsBox] = {
    new RuleTyperEvaluator[Environment, AstroutsBox](
      new IRuleTyperEvaluatorDelegate[Environment, AstroutsBox] {
        override def lookupType(state: AstroutsBox, env: Environment, name: String): (ITemplataType) = {
          val tyype = Astronomer.lookupType(state, env, name)
          (tyype)
        }
      })
  }

  def translateStruct(astrouts: AstroutsBox, env: Environment, structS: StructS): StructA = {
    val StructS(struct1Id, namespace, name, mutability, maybePredictedMutability, identifyingRunes, allRunes, predictedTypeByRune, isTemplate, rules, members) = structS

    // predictedTypeByRune is used by the rule typer delegate to short-circuit infinite recursion
    // in types like List, see RTMHTPS.
    val _ = predictedTypeByRune

    astrouts.getStruct(struct1Id) match {
      case Some(existingStructA) => return existingStructA
      case _ =>
    }

    val (conclusions, rulesA) =
      makeRuleTyper().solve(astrouts, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (c, RuleTyperSolveSuccess(r)) => (c, r)
      }

    val tyype =
      if (isTemplate) {
        TemplateTemplataType(identifyingRunes.map(conclusions.typeByRune), KindTemplataType)
      } else {
        KindTemplataType
      }

    val membersA =
      members.map({
        case StructMemberS(name, variablility, typeRune) => StructMemberA(name, variablility, typeRune)
      })

    StructA(struct1Id, namespace, name, mutability, maybePredictedMutability, tyype, identifyingRunes, conclusions.typeByRune, rulesA, membersA)
  }

  def translateInterface(astrouts: AstroutsBox, env: Environment, interfaceS: InterfaceS): InterfaceA = {
    val InterfaceS(interface1Id, namespace, name, mutability, maybePredictedMutability, identifyingRunes, allRunes, predictedTypeByRune, isTemplate, rules) = interfaceS

    // predictedTypeByRune is used by the rule typer delegate to short-circuit infinite recursion
    // in types like List, see RTMHTPS.
    val _ = predictedTypeByRune

    astrouts.getInterface(interface1Id) match {
      case Some(existingInterfaceA) => return existingInterfaceA
      case _ =>
    }

    val (conclusions, rulesA) =
      makeRuleTyper().solve(astrouts, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (c, RuleTyperSolveSuccess(r)) => (c, r)
      }

    val tyype =
      if (isTemplate) {
        TemplateTemplataType(identifyingRunes.map(conclusions.typeByRune), KindTemplataType)
      } else {
        KindTemplataType
      }

    val interfaceA =
      InterfaceA(
        interface1Id,
        namespace,
        name,
        mutability,
        maybePredictedMutability,
        tyype,
        identifyingRunes,
        conclusions.typeByRune,
        rulesA)
    interfaceA
  }

  def translateImpl(astrouts: AstroutsBox, env: Environment, implS: ImplS): ImplA = {
    val ImplS(codeLocation, rules, allRunes, isTemplate, structKindRune, interfaceKindRune) = implS

    astrouts.getImpl(codeLocation) match {
      case Some(existingImplA) => return existingImplA
      case _ =>
    }

    val (conclusions, rulesA) =
      makeRuleTyper().solve(astrouts, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (c, RuleTyperSolveSuccess(r)) => (c, r)
      }

    ImplA(codeLocation, rulesA, conclusions.typeByRune, structKindRune, interfaceKindRune)
  }

  def translateFunction(astrouts: AstroutsBox, env: Environment, functionS: FunctionS): FunctionA = {
    val FunctionS(codeLocation, name, namespace, lambdaNumber, isUserFunction, identifyingRunes, allRunes, maybePredictedType, paramsS, maybeRetCoordRune, isTemplate, templateRules, bodyS) = functionS

    val (conclusions, rulesA) =
      makeRuleTyper().solve(astrouts, env, templateRules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (c, RuleTyperSolveSuccess(r)) => (c, r)
      }

    val tyype =
      if (isTemplate) {
        TemplateTemplataType(identifyingRunes.map(conclusions.typeByRune), FunctionTemplataType)
      } else {
        FunctionTemplataType
      }

    val innerEnv = env.addRunes(conclusions.typeByRune)

    val bodyA = translateBody(astrouts, innerEnv, bodyS)

    FunctionA(
      codeLocation,
      name, namespace, lambdaNumber, isUserFunction,
      tyype,
      identifyingRunes, conclusions.typeByRune, paramsS, maybeRetCoordRune, rulesA, bodyA)
  }

  def translateBody(astrouts: AstroutsBox, env: Environment, body: IBody1): IBodyA = {
    body match {
      case ExternBody1 => ExternBodyA
      case AbstractBody1 => AbstractBodyA
      case GeneratedBody1(generatorId) => GeneratedBodyA(generatorId)
      case CodeBody1(BodySE(closuredNames, blockS)) => {
        val blockA = ExpressionAstronomer.translateBlock(env, astrouts, blockS)
        CodeBodyA(BodyAE(closuredNames, blockA))
      }
    }
  }

  def translateProgram(
      programS: ProgramS,
      primitives: Map[String, ITypeSR],
      suppliedFunctions: List[FunctionA]):
  ProgramA = {
    val ProgramS(structsS, interfacesS, implsS, functionsS) = programS

    val astrouts = AstroutsBox(Astrouts(Map(), Map(), Map(), Map()))

    val env = Environment(None, primitives, structsS, interfacesS, implsS, functionsS, Map())

    val structsA = structsS.map(translateStruct(astrouts, env, _))

    val interfacesA = interfacesS.map(translateInterface(astrouts, env, _))

    val implsA = implsS.map(translateImpl(astrouts, env, _))

    val functionsA = functionsS.map(translateFunction(astrouts, env, _))

    val _ = astrouts

    ProgramA(structsA, interfacesA, implsA, functionsA ++ suppliedFunctions)
  }


  val stlFunctions =
    Forwarders.forwarders ++
    List(
      NotEquals.function,
      Printing.printInt,
      Printing.printlnInt,
      Printing.printlnStr)

  val wrapperFunctions =
    List(
      Arrays.makeArrayFunction(MutableP),
      Arrays.makeArrayFunction(ImmutableP),
      RefCounting.checkmemberrc,
      RefCounting.checkvarrc)

  def runAstronomer(code: String): Option[ProgramA] = {
    Scout.runScout(code) match {
      case None => None
      case Some(programS) => {
        Some(runAstronomer(programS))
      }
    }
  }

  def runAstronomer(programS: ProgramS): ProgramA = {
    val suppliedFunctions = stlFunctions ++ wrapperFunctions ++ Forwarders.forwarders ++ Externs.externs
    val ProgramA(originalStructs, originalInterfaces, originalImpls, originalImplementedFunctionsS) =
      Astronomer.translateProgram(programS, primitives, suppliedFunctions)
    val programA =
      ProgramA(
        originalStructs,
        originalInterfaces,
        originalImpls,
        originalImplementedFunctionsS)
    programA
  }
}
