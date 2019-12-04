package net.verdagon.radonc.templar.citizen

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer}
import net.verdagon.radonc.{vassertSome, vcurious, vfail, vimpl}

import scala.collection.immutable.List

object StructTemplar {
  def addBuiltInStructs(env: NamespaceEnvironment, temputs0: Temputs): (Temputs, StructRef2) = {
    val structDef2 = StructDefinition2(FullName2(List(NamePart2("__Pack", Some(List())))), Immutable, List())
    val temputs1 = temputs0.declareStruct(structDef2.getRef)
    val temputs2 = temputs1.declareStructMutability(structDef2.getRef, Immutable)
    val temputs3 = temputs2.declareStructEnv(structDef2.getRef, env)
    val temputs4 = temputs3.add(structDef2)
    val temputs5 = temputs4.declarePack(List(), structDef2.getRef)

    (temputs5, structDef2.getRef)
  }

  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
      "templatedConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironment,
            temputs: Temputs,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (Temputs, FunctionHeader2) = {
            val Some(Coord(_, structRef2 @ StructRef2(_))) = maybeRetCoord
            val structDef2 = temputs.lookupStruct(structRef2)
            FunctionTemplarCore.makeConstructor(temputs, originFunction, structDef2)
          }
        })
  }

  def getConstructor(struct1: StructA):
  (FunctionA) = {
    println("todo: put all the members' rules up in the top of the struct")
    val params =
      struct1.members.zipWithIndex.map({
        case (member, index) => {
          ParameterS(AtomSP(Some(CaptureP(member.name, FinalP)), None, Scout.memberRunePrefix + index, None))
        }
      })
    val retRune = "__RetRune"
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
      GeneratedBodyA("templatedConstructorGenerator"))
  }

  def getStructRef(
    temputs0: Temputs,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(
      structTemplata.env, temputs0, structTemplata.originStruct, uncoercedTemplateArgs)
  }

  def getStructRef(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    struct1: StructA,
    uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(env, temputs0, struct1, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    temputs0: Temputs,
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(
      interfaceTemplata.env, temputs0, interfaceTemplata.originInterface, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    interface1: InterfaceA,
    uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(env, temputs0, interface1, uncoercedTemplateArgs)
  }

  def convert(
      env: IEnvironment,
      temputs0: Temputs,
      sourceExpr: ReferenceExpression2,
      sourceStructRef: StructRef2,
      targetInterfaceRef: InterfaceRef2):
  (Temputs, ReferenceExpression2) = {
    ImplTemplar.isAncestor(temputs0, sourceStructRef, targetInterfaceRef) match {
      case (temputs1, true) => (temputs1, StructToInterfaceUpcast2(sourceExpr, targetInterfaceRef))
      case (_, false) => vfail("Can't upcast a " + sourceStructRef + " to a " + targetInterfaceRef)
    }
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    env: IEnvironment,
    temputs0: Temputs,
    functionS: FunctionA,
    functionFullName: FullName2,
    members: List[StructMember2]):
  (Temputs, StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarTemplateArgsLayer.makeClosureUnderstruct(env, temputs0, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment, temputs0: Temputs, memberTypes2: List[Coord], prefix: String):
  (Temputs, StructRef2, Mutability) = {
    StructTemplarTemplateArgsLayer.makeSeqOrPackUnderstruct(env, temputs0, memberTypes2, prefix)
  }

  def getMemberCoords(temputs0: Temputs, structRef: StructRef2): List[Coord] = {
    temputs0.structDefsByRef(structRef).members.map(_.tyype).map({
      case ReferenceMemberType2(coord) => coord
      case AddressMemberType2(_) => {
        // At time of writing, the only one who calls this is the inferer, who wants to know so it
        // can match incoming arguments into a destructure. Can we even destructure things with
        // addressible members?
        vcurious()
      }
    })
  }

  def citizenIsFromTemplate(temputs: Temputs, citizen: CitizenRef2, template: ITemplata): (Temputs, Boolean) = {
    println("someday this is going to bite us")
    (citizen, template) match {
      case (InterfaceRef2(fullName), InterfaceTemplata(_, interfaceA)) => {
        if (interfaceA.namespace.nonEmpty || fullName.steps.size > 1) {
          return (temputs, false)
        }
        if (interfaceA.namespace.nonEmpty) {
          vimpl()
        }
        val lastStep = fullName.steps.last
        (temputs, lastStep.humanName == interfaceA.name)
      }
      case (StructRef2(fullName), StructTemplata(_, structA)) => {
        if (structA.namespace.size != fullName.steps.size - 1) {
          return (temputs, false)
        }
        if (structA.namespace.nonEmpty) {
          vimpl()
        }
        val lastStep = fullName.steps.last
        (temputs, lastStep.humanName == structA.name)
      }
      case _ => (temputs, false)
    }
  }
}
