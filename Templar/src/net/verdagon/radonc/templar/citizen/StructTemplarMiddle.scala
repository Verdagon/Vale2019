package net.verdagon.radonc.templar.citizen

import net.verdagon.radonc.astronomer.{FunctionA, InterfaceA, StructA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{IEnvironment, NamespaceEnvironment, TemplataEnvEntry}
import net.verdagon.radonc.templar.function.{FunctionTemplar, FunctionTemplarCore, VirtualTemplar}
import net.verdagon.radonc.vfail

import scala.collection.immutable.List

object StructTemplarMiddle {
  def getStructRef(
    structOuterEnv: NamespaceEnvironment,
    temputs0: Temputs,
    structS: StructA,
    templatasByRune: Map[String, ITemplata]):
  (Temputs, StructRef2) = {
    val coercedFinalTemplateArgs2 = structS.identifyingRunes.map(templatasByRune)

    val localEnv =
      structOuterEnv.addEntries(
        templatasByRune.mapValues(templata => List(TemplataEnvEntry(templata))))
    val (temputs2, structDefinition2) =
      StructTemplarCore.makeStruct(
        localEnv, temputs0, structS, coercedFinalTemplateArgs2);

    (temputs2, structDefinition2.getRef)
  }

  def getInterfaceRef(
    interfaceOuterEnv: NamespaceEnvironment,
    temputs0: Temputs,
    interfaceS: InterfaceA,
    templatasByRune: Map[String, ITemplata]):
  (Temputs, InterfaceRef2) = {
    val coercedFinalTemplateArgs2 = interfaceS.identifyingRunes.map(templatasByRune)

    val localEnv =
      interfaceOuterEnv.addEntries(
        templatasByRune.mapValues(templata => List(TemplataEnvEntry(templata))))
    val (temputs2, interfaceDefinition2) =
      StructTemplarCore.makeInterface(
        localEnv, temputs0, interfaceS, coercedFinalTemplateArgs2);

    val temputs10 = temputs2

// Now that we have an env, we can use it for the internal methods.
//      interfaceS.internalMethods.foldLeft(temputs2)({
//        case (temputs7, function) => {
//          FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
//            temputs7, FunctionTemplata(localEnv, function))
//        }
//      })

    (temputs10, interfaceDefinition2.getRef)
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    env: IEnvironment,
    temputs0: Temputs,
    functionS: FunctionA,
    functionFullName: FullName2,
    members: List[StructMember2]):
  (Temputs, StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarCore.makeClosureUnderstruct(env, temputs0, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    memberTypes2: List[Coord],
    prefix: String):
  (Temputs, StructRef2, Mutability) = {
    StructTemplarCore.makeSeqOrPackUnderstruct(env, temputs0, memberTypes2, prefix)
  }
}
