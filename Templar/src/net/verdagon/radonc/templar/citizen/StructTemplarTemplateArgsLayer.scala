package net.verdagon.radonc.templar.citizen

import net.verdagon.radonc.astronomer.{FunctionA, InterfaceA, StructA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.function.FunctionTemplar
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.vfail

import scala.collection.immutable.List

object StructTemplarTemplateArgsLayer {

  def getStructRef(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    structS: StructA,
    templateArgs: List[ITemplata]):
  (Temputs, StructRef2) = {
    val fullName = FullName2(env.fullName.steps :+ NamePart2(structS.name, Some(templateArgs)))

    temputs0.structDeclared(fullName) match {
      case Some(structRef2) => {
        (temputs0, structRef2)
      }
      case None => {
        // not sure if this is okay or not, do we allow this?
        if (templateArgs.size != structS.identifyingRunes.size) {
          vfail("wat?")
        }
        val temporaryStructRef = StructRef2(fullName)
        val temputs1 = temputs0.declareStruct(temporaryStructRef)

        val temputs2 =
          structS.maybePredictedMutability match {
            case None => temputs1
            case Some(predictedMutability) => temputs1.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(predictedMutability))
          }

        val (temputs3, result) =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs2,
            structS.identifyingRunes,
            structS.rules,
            structS.typeByRune,
            List(),
            None,
            templateArgs)

        val inferences =
          result match {
            case isf @ InferSolveFailure(_, _, _, _, _, _) => {
              vfail("Couldnt figure out template args! Cause:\n" + isf)
            }
            case InferSolveSuccess(i) => i
          }

        val temputs4 =
          structS.maybePredictedMutability match {
            case None => temputs3.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(structS.mutability))
            case Some(_) => temputs3
          }

        StructTemplarMiddle.getStructRef(env, temputs4, structS, inferences.templatasByRune)
      }
    }
  }

  def getInterfaceRef(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    interfaceS: InterfaceA,
    templateArgs: List[ITemplata]):
  (Temputs, InterfaceRef2) = {
    val fullName = FullName2(env.fullName.steps :+ NamePart2(interfaceS.name, Some(templateArgs)))

    temputs0.interfaceDeclared(fullName) match {
      case Some(interfaceRef2) => {
        (temputs0, interfaceRef2)
      }
      case None => {
        // not sure if this is okay or not, do we allow this?
        if (templateArgs.size != interfaceS.identifyingRunes.size) {
          vfail("wat?")
        }
        val temporaryInterfaceRef = InterfaceRef2(fullName)
        val temputs1 = temputs0.declareInterface(temporaryInterfaceRef)

        val temputs2 =
          interfaceS.maybePredictedMutability match {
            case None => temputs1
            case Some(predictedMutability) => temputs1.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(predictedMutability))
          }

        val (temputs3, result) =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs2,
            interfaceS.identifyingRunes,
            interfaceS.rules,
            interfaceS.typeByRune,
            List(),
            None,
            templateArgs)
        val inferences =
          result match {
            case isf @ InferSolveFailure(_, _, _, _, _, _) => {
              vfail("Couldnt figure out template args! Cause:\n" + isf)
            }
            case InferSolveSuccess(i) => i
          }

        val temputs4 =
          interfaceS.maybePredictedMutability match {
            case None => temputs3.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(interfaceS.mutability))
            case Some(_) => temputs3
          }

        StructTemplarMiddle.getInterfaceRef(env, temputs4, interfaceS, inferences.templatasByRune)
      }
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
    StructTemplarMiddle.makeClosureUnderstruct(env, temputs0, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment, temputs0: Temputs, memberTypes2: List[Coord], prefix: String):
  (Temputs, StructRef2, Mutability) = {
    StructTemplarMiddle.makeSeqOrPackUnderstruct(env, temputs0, memberTypes2, prefix)
  }
}
