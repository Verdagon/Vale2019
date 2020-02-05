package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer}
import net.verdagon.vale._

import scala.collection.immutable.List

object StructTemplar {
  def addBuiltInStructs(env: NamespaceEnvironment[IName2], temputs: TemputsBox): (StructRef2) = {
    val structDef2 = StructDefinition2(FullName2(List(PackName2(List()))), Immutable, List(), false)
    temputs.declareStruct(structDef2.getRef)
    temputs.declareStructMutability(structDef2.getRef, Immutable)
    temputs.declareStructEnv(structDef2.getRef, env)
    temputs.add(structDef2)
    temputs.declarePack(List(), structDef2.getRef)
    (structDef2.getRef)
  }

  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
      "structConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironment,
            temputs: TemputsBox,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (FunctionHeader2) = {
            val Some(Coord(_, structRef2 @ StructRef2(_))) = maybeRetCoord
            val structDef2 = temputs.lookupStruct(structRef2)
            StructTemplarCore.makeStructConstructor(temputs, originFunction, structDef2, env.fullName)
          }
        },
      "interfaceConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironment,
            temputs: TemputsBox,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (FunctionHeader2) = {
            // The interface should be in the "__Interface" rune of the function environment.
            val interfaceRef2 =
              env.getNearestTemplataWithName(StructTemplar.anonymousSubstructParentInterfaceRune, Set(TemplataLookupContext)) match {
                case Some(KindTemplata(ir @ InterfaceRef2(_))) => ir
                case _ => vwat()
              }
            val (_, _, constructor) =
              StructTemplar.makeAnonymousSubstruct(
                env, temputs, originFunction, env.fullName, interfaceRef2, paramCoords.map(_.tyype))
            constructor
          }
        })
  }

  def getConstructor(struct1: StructA): FunctionA = {
    println("todo: put all the members' rules up in the top of the struct")
    val params =
      struct1.members.zipWithIndex.map({
        case (member, index) => {
          ParameterS(AtomSP(Some(CaptureP(member.name, FinalP)), None, MemberRune(struct1.codeLocation, index), None))
        }
      })
    val rules =
      struct1.rules :+
      EqualsAR(
        TemplexAR(RuneAT(retRune, CoordTemplataType)),
        TemplexAR(
          if (struct1.isTemplate) {
            CallAT(
              NameAT(struct1.name, struct1.tyype),
              struct1.identifyingRunes.map(rune => RuneAT(rune, struct1.typeByRune(rune))),
              CoordTemplataType)
          } else {
            NameAT(struct1.name, CoordTemplataType)
          }))

    FunctionA(
      struct1.codeLocation,
      struct1.name,
      struct1.namespace,
      0,
      true,
      struct1.tyype match {
        case KindTemplataType => FunctionTemplataType
        case TemplateTemplataType(params, KindTemplataType) => TemplateTemplataType(params, FunctionTemplataType)
      },
      struct1.identifyingRunes,
      struct1.typeByRune + (retRune -> CoordTemplataType),
      params,
      Some(retRune),
      rules,
      GeneratedBodyA("structConstructorGenerator"))
  }

  def getInterfaceConstructor(interfaceA: InterfaceA): FunctionA = {
    println("todo: put all the members' rules up in the top of the struct")
    val identifyingRunes =
      interfaceA.identifyingRunes ++ interfaceA.internalMethods.indices.map("Functor" + _)
    val typeByRune =
      interfaceA.typeByRune ++
      interfaceA.internalMethods.indices.map(i => (("Functor" + i) -> CoordTemplataType)).toMap
    val params =
      interfaceA.internalMethods.indices.toList.map(index => {
        ParameterS(AtomSP(Some(CaptureP("functor" + index, FinalP)), None, "Functor" + index, None))
      })
    val rules =
      interfaceA.rules :+
//        EqualsAR(
//          TemplexAR(RuneAT(retRune, CoordTemplataType)),
//          TemplexAR(
//            if (interfaceA.isTemplate) {
//              CallAT(
//                NameAT(interfaceA.name, interfaceA.tyype),
//                interfaceA.identifyingRunes.map(rune => RuneAT(rune, interfaceA.typeByRune(rune))),
//                CoordTemplataType)
//            } else {
//              NameAT(interfaceA.name, CoordTemplataType)
//            })) :+
    // We stash the interface type in the env, so that when the interface constructor generator runs,
    // it can read this to know what interface it's making a subclass of.
      EqualsAR(
        TemplexAR(RuneAT(StructTemplar.anonymousSubstructParentInterfaceRune, KindTemplataType)),
        TemplexAR(
          if (interfaceA.isTemplate) {
            CallAT(
              NameAT(interfaceA.name, interfaceA.tyype),
              interfaceA.identifyingRunes.map(rune => RuneAT(rune, interfaceA.typeByRune(rune))),
              KindTemplataType)
          } else {
            NameAT(interfaceA.name, KindTemplataType)
          }))

    FunctionA(
      interfaceA.codeLocation,
      interfaceA.name,
      interfaceA.namespace,
      0,
      true,
      interfaceA.tyype match {
        case KindTemplataType => FunctionTemplataType
        case TemplateTemplataType(params, KindTemplataType) => TemplateTemplataType(params, FunctionTemplataType)
      },
      identifyingRunes,
      typeByRune,
      params,
      None,
      rules,
      GeneratedBodyA("interfaceConstructorGenerator"))
  }

  def getStructRef(
    temputs: TemputsBox,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(
      temputs, structTemplata, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    temputs: TemputsBox,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(
      temputs, interfaceTemplata, uncoercedTemplateArgs)
  }

  def convert(
      env: IEnvironment,
      temputs: TemputsBox,
      sourceExpr: ReferenceExpression2,
      sourceStructRef: StructRef2,
      targetInterfaceRef: InterfaceRef2):
  (ReferenceExpression2) = {
    ImplTemplar.isAncestor(temputs, sourceStructRef, targetInterfaceRef) match {
      case (true) => (StructToInterfaceUpcast2(sourceExpr, targetInterfaceRef))
      case (false) => vfail("Can't upcast a " + sourceStructRef + " to a " + targetInterfaceRef)
    }
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    env: IEnvironment,
    temputs: TemputsBox,
    functionS: FunctionA,
    functionFullName: FullName2[IFunctionName2],
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarTemplateArgsLayer.makeClosureUnderstruct(env, temputs, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment[IName2], temputs: TemputsBox, memberTypes2: List[Coord], prefix: String):
  (StructRef2, Mutability) = {
    StructTemplarTemplateArgsLayer.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, prefix)
  }

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    functionFullName: FullName2[IFunctionName2],
    interfaceRef: InterfaceRef2,
    lambdas: List[Coord]):
  (StructRef2, Mutability, FunctionHeader2) = {
    StructTemplarTemplateArgsLayer.makeAnonymousSubstruct(
      outerEnv, temputs, maybeConstructorOriginFunctionA, functionFullName, interfaceRef, lambdas)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  def prototypeToAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    interfaceRef: InterfaceRef2,
    prototype: Prototype2):
  StructRef2 = {
    StructTemplarTemplateArgsLayer.prototypeToAnonymousSubstruct(
      outerEnv, temputs, interfaceRef, prototype)
  }

//  // Makes a functor for the given prototype.
//  def functionToLambda(
//    outerEnv: IEnvironment,
//    temputs: TemputsBox,
//    header: FunctionHeader2):
//  StructRef2 = {
//    StructTemplarTemplateArgsLayer.functionToLambda(outerEnv, temputs, header)
//  }

  def getMemberCoords(temputs: TemputsBox, structRef: StructRef2): List[Coord] = {
    temputs.structDefsByRef(structRef).members.map(_.tyype).map({
      case ReferenceMemberType2(coord) => coord
      case AddressMemberType2(_) => {
        // At time of writing, the only one who calls this is the inferer, who wants to know so it
        // can match incoming arguments into a destructure. Can we even destructure things with
        // addressible members?
        vcurious()
      }
    })
  }

  def citizenIsFromTemplate(temputs: TemputsBox, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
    println("someday this is going to bite us")
    (citizen, template) match {
      case (InterfaceRef2(fullName), InterfaceTemplata(_, interfaceA)) => {
        if (interfaceA.namespace.nonEmpty || fullName.steps.size > 1) {
          return (false)
        }
        if (interfaceA.namespace.nonEmpty) {
          vimpl()
        }
        val lastStep = fullName.last
        (lastStep.humanName == interfaceA.name)
      }
      case (StructRef2(fullName), StructTemplata(_, structA)) => {
        if (structA.namespace.size != fullName.steps.size - 1) {
          return (false)
        }
        if (structA.namespace.nonEmpty) {
          vimpl()
        }
        val lastStep = fullName.last
        (lastStep.humanName == structA.name)
      }
      case _ => (false)
    }
  }

//  def headerToIFunctionSubclass(
//    env: IEnvironment,
//    temputs: TemputsBox,
//    header: FunctionHeader2):
//  StructRef2 = {
//    val (paramType, returnType) =
//      header.toPrototype match {
//        case Prototype2(_, List(paramType), returnType) => (paramType, returnType)
//        case _ => vimpl("Only IFunction1 implemented")
//      }
//    val Some(InterfaceTemplata(ifunction1InterfaceEnv, ifunction1InterfaceA)) =
//      env.getNearestTemplataWithName("IFunction1", Set(TemplataLookupContext))
//
//    val lambdaStructRef = StructTemplar.functionToLambda(env, temputs, header)
//
//    val ifunction1InterfaceRef =
//      StructTemplar.getInterfaceRef(
//        ifunction1InterfaceEnv,
//        temputs,
//        ifunction1InterfaceA,
//        List(
//          MutabilityTemplata(Immutable),
//          CoordTemplata(paramType),
//          CoordTemplata(returnType)))
//
//    makeAnonymousSubstruct()
//  }

  def prototypeToAnonymousIFunctionSubstruct(
      env: IEnvironment,
      temputs: TemputsBox,
    prototype: Prototype2):
  (StructRef2, InterfaceRef2) = {
    val Prototype2(_, List(paramType), returnType) = prototype

    val Some(ifunction1Templata@InterfaceTemplata(_, _)) =
      env.getNearestTemplataWithName("IFunction1", Set(TemplataLookupContext))
    val ifunction1InterfaceRef =
      StructTemplar.getInterfaceRef(
        temputs,
        ifunction1Templata,
        List(
          MutabilityTemplata(Immutable),
          CoordTemplata(paramType),
          CoordTemplata(returnType)))

    val elementDropFunctionAsIFunctionSubstructStructRef =
      StructTemplar.prototypeToAnonymousSubstruct(
        env, temputs, ifunction1InterfaceRef, prototype)

    (elementDropFunctionAsIFunctionSubstructStructRef, ifunction1InterfaceRef)
  }
}
