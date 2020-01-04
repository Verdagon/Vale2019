package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.{FunctionA, InterfaceA, StructA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{IEnvironment, NamespaceEnvironment, TemplataEnvEntry}
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, VirtualTemplar}
import net.verdagon.vale.vfail

import scala.collection.immutable.List

object StructTemplarMiddle {
  def getStructRef(
    structOuterEnv: NamespaceEnvironment,
    temputs: TemputsBox,
    structS: StructA,
    templatasByRune: Map[String, ITemplata]):
  (StructRef2) = {
    val coercedFinalTemplateArgs2 = structS.identifyingRunes.map(templatasByRune)

    val localEnv =
      structOuterEnv.addEntries(
        templatasByRune.mapValues(templata => List(TemplataEnvEntry(templata))))
    val structDefinition2 =
      StructTemplarCore.makeStruct(
        localEnv, temputs, structS, coercedFinalTemplateArgs2);

    (structDefinition2.getRef)
  }

  def getInterfaceRef(
    interfaceOuterEnv: NamespaceEnvironment,
    temputs: TemputsBox,
    interfaceS: InterfaceA,
    templatasByRune: Map[String, ITemplata]):
  (InterfaceRef2) = {
    val coercedFinalTemplateArgs2 = interfaceS.identifyingRunes.map(templatasByRune)

    val localEnv =
      interfaceOuterEnv.addEntries(
        templatasByRune.mapValues(templata => List(TemplataEnvEntry(templata))))
    val interfaceDefinition2 =
      StructTemplarCore.makeInterface(
        localEnv, temputs, interfaceS, coercedFinalTemplateArgs2);

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
    env: IEnvironment,
    temputs: TemputsBox,
    functionS: FunctionA,
    functionFullName: FullName2,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarCore.makeClosureUnderstruct(env, temputs, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    memberTypes2: List[Coord],
    prefix: String):
  (StructRef2, Mutability) = {
    StructTemplarCore.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, prefix)
  }

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    functionFullName: FullName2,
    interfaceRef: InterfaceRef2,
    lambdas: List[Coord]):
  (StructRef2, Mutability, FunctionHeader2) = {
    StructTemplarCore.makeAnonymousSubstruct(outerEnv, temputs, maybeConstructorOriginFunctionA, functionFullName, interfaceRef, lambdas)
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
