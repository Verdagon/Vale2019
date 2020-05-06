package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.{FunctionA, InterfaceA, LambdaNameA, StructA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{IEnvironment, InterfaceEnvEntry, NamespaceEnvironment, TemplataEnvEntry}
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, VirtualTemplar}
import net.verdagon.vale.{vfail, vimpl}

import scala.collection.immutable.List

object StructTemplarMiddle {
  def getStructRef(
    structOuterEnv: NamespaceEnvironment[IName2],
    temputs: TemputsBox,
    structS: StructA,
    templatasByRune: Map[IRune2, ITemplata]):
  (StructRef2) = {
    val coercedFinalTemplateArgs2 = structS.identifyingRunes.map(NameTranslator.translateRune).map(templatasByRune)

    val localEnv =
      structOuterEnv.addEntries(
        templatasByRune.map({ case (rune, templata) => (rune, List(TemplataEnvEntry(templata))) }))
    val structDefinition2 =
      StructTemplarCore.makeStruct(
        localEnv, temputs, structS, coercedFinalTemplateArgs2);

    (structDefinition2.getRef)
  }

  def getInterfaceRef(
    interfaceOuterEnv: NamespaceEnvironment[IName2],
    temputs: TemputsBox,
    interfaceA: InterfaceA,
    templatasByRune: Map[IRune2, ITemplata]):
  (InterfaceRef2) = {
    val coercedFinalTemplateArgs2 = interfaceA.identifyingRunes.map(NameTranslator.translateRune).map(templatasByRune)

    val localEnv =
      interfaceOuterEnv.addEntries(
        templatasByRune.map({ case (rune, templata) => (rune, List(TemplataEnvEntry(templata))) }))
    val interfaceDefinition2 =
      StructTemplarCore.makeInterface(
        localEnv, temputs, interfaceA, coercedFinalTemplateArgs2);

// Now that we have an env, we can use it for the internal methods.
//      interfaceS.internalMethods.foldLeft(temputs)({
//        case (function) => {
//          FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
//            temputs, FunctionTemplata(localEnv, function))
//        }
//      })

    (interfaceDefinition2.getRef)
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: TemputsBox,
    name: LambdaNameA,
    functionS: FunctionA,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarCore.makeClosureUnderstruct(containingFunctionEnv, temputs, name, functionS, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(
    env: NamespaceEnvironment[IName2],
    temputs: TemputsBox,
    memberTypes2: List[Coord],
    name: IStructName2):
  (StructRef2, Mutability) = {
    StructTemplarCore.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, name)
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

    vimpl()
//    maybe this?:
//    val anonymousSubstructName: FullName2[AnonymousSubstructName2] =
//      functionFullName.addStep(AnonymousSubstructName2())
    // but we do need some sort of codelocation in there, right?
    // otherwise i can say IMyInterface(sum, mul) + IMyInterface(sum, mul)
    // actually thats probably fine. we would just reuse the existing one.
    // ...we best write a doc section on this.

    StructTemplarCore.makeAnonymousSubstruct(
      outerEnv,
      temputs,
      maybeConstructorOriginFunctionA,
      functionFullName,
      interfaceRef,
      lambdas)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  def prototypeToAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    interfaceRef: InterfaceRef2,
    prototype: Prototype2):
  StructRef2 = {
    StructTemplarCore.prototypeToAnonymousSubstruct(
      outerEnv, temputs, interfaceRef, prototype)
  }
}
