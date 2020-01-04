package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.{FunctionA, InterfaceA, StructA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.vale.templar.function.FunctionTemplar
import net.verdagon.vale.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.vfail

import scala.collection.immutable.List

object StructTemplarTemplateArgsLayer {

  def getStructRef(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    structS: StructA,
    templateArgs: List[ITemplata]):
  (StructRef2) = {
    val fullName = FullName2(env.fullName.steps :+ NamePart2(structS.name, Some(templateArgs), None, None))

    temputs.structDeclared(fullName) match {
      case Some(structRef2) => {
        (structRef2)
      }
      case None => {
        // not sure if this is okay or not, do we allow this?
        if (templateArgs.size != structS.identifyingRunes.size) {
          vfail("wat?")
        }
        val temporaryStructRef = StructRef2(fullName)
        temputs.declareStruct(temporaryStructRef)


          structS.maybePredictedMutability match {
            case None => temputs
            case Some(predictedMutability) => temputs.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(predictedMutability))
          }

        val result =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs,
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


          structS.maybePredictedMutability match {
            case None => temputs.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(structS.mutability))
            case Some(_) => temputs
          }

        StructTemplarMiddle.getStructRef(env, temputs, structS, inferences.templatasByRune)
      }
    }
  }

  def getInterfaceRef(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    interfaceS: InterfaceA,
    templateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    val fullName = FullName2(env.fullName.steps :+ NamePart2(interfaceS.name, Some(templateArgs), None, None))

    temputs.interfaceDeclared(fullName) match {
      case Some(interfaceRef2) => {
        (interfaceRef2)
      }
      case None => {
        // not sure if this is okay or not, do we allow this?
        if (templateArgs.size != interfaceS.identifyingRunes.size) {
          vfail("wat?")
        }
        val temporaryInterfaceRef = InterfaceRef2(fullName)
        temputs.declareInterface(temporaryInterfaceRef)


          interfaceS.maybePredictedMutability match {
            case None => temputs
            case Some(predictedMutability) => temputs.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(predictedMutability))
          }

        val result =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs,
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


          interfaceS.maybePredictedMutability match {
            case None => temputs.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(interfaceS.mutability))
            case Some(_) => temputs
          }

        StructTemplarMiddle.getInterfaceRef(env, temputs, interfaceS, inferences.templatasByRune)
      }
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
    StructTemplarMiddle.makeClosureUnderstruct(env, temputs, functionS, functionFullName, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment, temputs: TemputsBox, memberTypes2: List[Coord], prefix: String):
  (StructRef2, Mutability) = {
    StructTemplarMiddle.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, prefix)
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
    StructTemplarMiddle.makeAnonymousSubstruct(outerEnv, temputs, maybeConstructorOriginFunctionA, functionFullName, interfaceRef, lambdas)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  def prototypeToAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    interfaceRef: InterfaceRef2,
    prototype: Prototype2):
  StructRef2 = {
    StructTemplarMiddle.prototypeToAnonymousSubstruct(
      outerEnv, temputs, interfaceRef, prototype)
  }
}
