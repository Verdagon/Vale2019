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

  def translateStructs(astrouts0: Astrouts, env: Environment, structsS: List[StructS]): (Astrouts, List[StructA]) = {
    structsS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = translateStruct(astrouts0, env, headS)
        val (astrouts2, tailA) = translateStructs(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
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

  def lookupStructType(astrouts0: Astrouts, env: Environment, structS: StructS):
  (Astrouts, ITemplataType) = {
    structS.maybePredictedType match {
      case Some(predictedType) => {
        (astrouts0, translateRuneType(predictedType))
      }
      case None => {
        val (astrouts1, structA) = translateStruct(astrouts0, env, structS)
        (astrouts1, structA.tyype)
      }
    }
  }

  def lookupStructTypes(astrouts0: Astrouts, env: Environment, structsS: List[StructS]):
  (Astrouts, List[ITemplataType]) = {
    structsS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = lookupStructType(astrouts0, env, headS)
        val (astrouts2, tailA) = lookupStructTypes(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
    }
  }

  def lookupInterfaceType(astrouts0: Astrouts, env: Environment, interfaceS: InterfaceS):
  (Astrouts, ITemplataType) = {
    interfaceS.maybePredictedType match {
      case Some(predictedType) => {
        (astrouts0, translateRuneType(predictedType))
      }
      case None => {
        val (astrouts1, interfaceA) = translateInterface(astrouts0, env, interfaceS)
        (astrouts1, interfaceA.tyype)
      }
    }
  }

  def lookupInterfaceTypes(astrouts0: Astrouts, env: Environment, interfacesS: List[InterfaceS]):
  (Astrouts, List[ITemplataType]) = {
    interfacesS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = lookupInterfaceType(astrouts0, env, headS)
        val (astrouts2, tailA) = lookupInterfaceTypes(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
    }
  }

  def lookupType(astrouts0: Astrouts, env: Environment, name: String): (Astrouts, ITemplataType) = {
    // When the scout comes across a lambda, it doesn't put the e.g. __Closure:main:lam1 struct into
    // the environment or anything, it lets templar to do that (because templar knows the actual types).
    // However, this means that when the lambda function gets to the astronomer, the astronomer doesn't
    // know what to do with it.
    // TODO: Make it so names aren't strings, but instead sealed traits and case classes.
    // Or, figure out something else to avoid this oddity here.
    if (name.startsWith(FunctionScout.CLOSURE_STRUCT_NAME)) {
      return (astrouts0, KindTemplataType)
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
      (astrouts0, translateRuneType(primitivesS.get))
    } else if (maybeRuneTypeS.nonEmpty) {
      (astrouts0, maybeRuneTypeS.get)
    } else if (structsS.nonEmpty) {
      val (astrouts1, types) = lookupStructTypes(astrouts0, env, structsS)
      if (types.toSet.size > 1) {
        vfail("'" + name + "' has multiple types: " + types.toSet)
      }
      val tyype = types.head
      (astrouts1, tyype)
    } else if (interfacesS.nonEmpty) {
      val (astrouts1, types) = lookupInterfaceTypes(astrouts0, env, interfacesS)
      if (types.toSet.size > 1) {
        vfail("'" + name + "' has multiple types: " + types.toSet)
      }
      val tyype = types.head
      (astrouts1, tyype)
    } else vfail()
  }

  def makeRuleTyper(): RuleTyperEvaluator[Environment, Astrouts] = {
    new RuleTyperEvaluator[Environment, Astrouts](
      new IRuleTyperEvaluatorDelegate[Environment, Astrouts] {
        override def lookupType(state: Astrouts, env: Environment, name: String): (Astrouts, ITemplataType) = {
          Astronomer.lookupType(state, env, name)
        }
      })
  }

  def translateStruct(astrouts0: Astrouts, env: Environment, structS: StructS): (Astrouts, StructA) = {
    val StructS(struct1Id, namespace, name, mutability, maybePredictedMutability, identifyingRunes, allRunes, predictedTypeByRune, isTemplate, rules, members) = structS

    // predictedTypeByRune is used by the rule typer delegate to short-circuit infinite recursion
    // in types like List, see RTMHTPS.
    val (_) = predictedTypeByRune

    astrouts0.structs.get(struct1Id) match {
      case Some(existingStructA) => return (astrouts0, existingStructA)
      case _ =>
    }

    val (astrouts2, conclusions, rulesA) =
      makeRuleTyper().solve(astrouts0, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (astrouts1, RuleTyperSolveSuccess(c, r)) => (astrouts1, c, r)
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

    val structA =
      StructA(struct1Id, namespace, name, mutability, maybePredictedMutability, tyype, identifyingRunes, conclusions.typeByRune, rulesA, membersA)
    (astrouts2, structA)
  }

  def translateInterfaces(astrouts0: Astrouts, env: Environment, interfacesS: List[InterfaceS]): (Astrouts, List[InterfaceA]) = {
    interfacesS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = translateInterface(astrouts0, env, headS)
        val (astrouts2, tailA) = translateInterfaces(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
    }
  }

  def translateInterface(astrouts0: Astrouts, env: Environment, interfaceS: InterfaceS): (Astrouts, InterfaceA) = {
    val InterfaceS(interface1Id, namespace, name, mutability, maybePredictedMutability, identifyingRunes, allRunes, predictedTypeByRune, isTemplate, rules) = interfaceS

    // predictedTypeByRune is used by the rule typer delegate to short-circuit infinite recursion
    // in types like List, see RTMHTPS.
    val (_) = predictedTypeByRune

    astrouts0.interfaces.get(interface1Id) match {
      case Some(existingInterfaceA) => return (astrouts0, existingInterfaceA)
      case _ =>
    }

    val (astrouts2, conclusions, rulesA) =
      makeRuleTyper().solve(astrouts0, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (astrouts1, RuleTyperSolveSuccess(c, r)) => (astrouts1, c, r)
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
    (astrouts2, interfaceA)
  }

  def translateImpl(astrouts0: Astrouts, env: Environment, implS: ImplS): (Astrouts, ImplA) = {
    val ImplS(codeLocation, rules, allRunes, isTemplate, structKindRune, interfaceKindRune) = implS

    astrouts0.impls.get(codeLocation) match {
      case Some(existingImplA) => return (astrouts0, existingImplA)
      case _ =>
    }

    val (astrouts2, conclusions, rulesA) =
      makeRuleTyper().solve(astrouts0, env, rules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (astrouts1, RuleTyperSolveSuccess(c, r)) => (astrouts1, c, r)
      }

    val implA =
      ImplA(codeLocation, rulesA, conclusions.typeByRune, structKindRune, interfaceKindRune)
    (astrouts2, implA)
  }

  def translateImpls(astrouts0: Astrouts, env: Environment, implS: List[ImplS]): (Astrouts, List[ImplA]) = {
    implS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = translateImpl(astrouts0, env, headS)
        val (astrouts2, tailA) = translateImpls(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
    }
  }

  def translateFunction(astrouts0: Astrouts, env: Environment, functionS: FunctionS): (Astrouts, FunctionA) = {
    val FunctionS(codeLocation, name, namespace, lambdaNumber, isUserFunction, identifyingRunes, allRunes, maybePredictedType, paramsS, maybeRetCoordRune, isTemplate, templateRules, bodyS) = functionS

    val (astrouts2, conclusions, rulesA) =
      makeRuleTyper().solve(astrouts0, env, templateRules, List(), Some(allRunes)) match {
        case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
        case (astrouts1, RuleTyperSolveSuccess(c, r)) => (astrouts1, c, r)
      }

    val tyype =
      if (isTemplate) {
        TemplateTemplataType(identifyingRunes.map(conclusions.typeByRune), FunctionTemplataType)
      } else {
        FunctionTemplataType
      }

    val innerEnv = env.addRunes(conclusions.typeByRune)

    val (astrouts3, bodyA) = translateBody(astrouts2, innerEnv, bodyS)

    val functionA =
      FunctionA(
        codeLocation,
        name, namespace, lambdaNumber, isUserFunction,
        tyype,
        identifyingRunes, conclusions.typeByRune, paramsS, maybeRetCoordRune, rulesA, bodyA)
    (astrouts3, functionA)
  }

  def translateBody(astrouts0: Astrouts, env: Environment, body: IBody1): (Astrouts, IBodyA) = {
    body match {
      case ExternBody1 => (astrouts0, ExternBodyA)
      case AbstractBody1 => (astrouts0, AbstractBodyA)
      case GeneratedBody1(generatorId) => (astrouts0, GeneratedBodyA(generatorId))
      case CodeBody1(BodySE(closuredNames, blockS)) => {
        val (astrouts10, blockA) = ExpressionAstronomer.translateBlock(env, astrouts0, blockS)
        (astrouts10, CodeBodyA(BodyAE(closuredNames, blockA)))
      }
    }
  }

  def translateFunctions(astrouts0: Astrouts, env: Environment, functionsS: List[FunctionS]): (Astrouts, List[FunctionA]) = {
    functionsS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts1, headA) = translateFunction(astrouts0, env, headS)
        val (astrouts2, tailA) = translateFunctions(astrouts1, env, tailS)
        (astrouts2, headA :: tailA)
      }
    }
  }

  def translateProgram(
      programS: ProgramS,
      primitives: Map[String, ITypeSR],
      suppliedFunctions: List[FunctionA]):
  ProgramA = {
    val ProgramS(structsS, interfacesS, implsS, functionsS) = programS

    val astrouts0 = Astrouts(Map(), Map(), Map(), Map())

    val env = Environment(None, primitives, structsS, interfacesS, implsS, functionsS, Map())

    val (astrouts1, structsA) = translateStructs(astrouts0, env, structsS)

    val (astrouts2, interfacesA) = translateInterfaces(astrouts1, env, interfacesS)

    val (astrouts3, implsA) = translateImpls(astrouts2, env, implsS)

    val (astrouts4, functionsA) = translateFunctions(astrouts3, env, functionsS)

    val (_) = astrouts4

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
