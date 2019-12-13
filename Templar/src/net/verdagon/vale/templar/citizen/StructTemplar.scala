package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{FunctionEnvironment, FunctionEnvironmentBox, IEnvironment, NamespaceEnvironment}
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer}
import net.verdagon.vale.{vassertSome, vcurious, vfail, vimpl}

import scala.collection.immutable.List

object StructTemplar {
  def addBuiltInStructs(env: NamespaceEnvironment, temputs: TemputsBox): (StructRef2) = {
    val structDef2 = StructDefinition2(FullName2(List(NamePart2("__Pack", Some(List())))), Immutable, List())
    temputs.declareStruct(structDef2.getRef)
    temputs.declareStructMutability(structDef2.getRef, Immutable)
    temputs.declareStructEnv(structDef2.getRef, env)
    temputs.add(structDef2)
    temputs.declarePack(List(), structDef2.getRef)
    (structDef2.getRef)
  }

  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
      "templatedConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironmentBox,
            temputs: TemputsBox,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (FunctionHeader2) = {
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
    temputs: TemputsBox,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(
      structTemplata.env, temputs, structTemplata.originStruct, uncoercedTemplateArgs)
  }

  def getStructRef(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    struct1: StructA,
    uncoercedTemplateArgs: List[ITemplata]):
  (StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(env, temputs, struct1, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    temputs: TemputsBox,
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(
      interfaceTemplata.env, temputs, interfaceTemplata.originInterface, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    interface1: InterfaceA,
    uncoercedTemplateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(env, temputs, interface1, uncoercedTemplateArgs)
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
    functionFullName: FullName2,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarTemplateArgsLayer.makeClosureUnderstruct(env, temputs, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment, temputs: TemputsBox, memberTypes2: List[Coord], prefix: String):
  (StructRef2, Mutability) = {
    StructTemplarTemplateArgsLayer.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, prefix)
  }

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
        val lastStep = fullName.steps.last
        (lastStep.humanName == interfaceA.name)
      }
      case (StructRef2(fullName), StructTemplata(_, structA)) => {
        if (structA.namespace.size != fullName.steps.size - 1) {
          return (false)
        }
        if (structA.namespace.nonEmpty) {
          vimpl()
        }
        val lastStep = fullName.steps.last
        (lastStep.humanName == structA.name)
      }
      case _ => (false)
    }
  }
}
