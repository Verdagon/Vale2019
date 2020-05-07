package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.FunctionTemplar
import net.verdagon.vale.templar.infer.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.{vfail, vimpl}

import scala.collection.immutable.List

object StructTemplarTemplateArgsLayer {

  def translateStructNameWithTemplateArgs(nameA: INameA, templateArgs: List[ITemplata]): IStructName2 = {
    nameA match {
      case _ => vimpl()
    }
  }

  def translateInterfaceNameWithTemplateArgs(nameA: INameA, templateArgs: List[ITemplata]): InterfaceName2 = {
    nameA match {
      case _ => vimpl()
    }
  }

  def getStructRef(
    temputs: TemputsBox,
    structTemplata: StructTemplata,
    templateArgs: List[ITemplata]):
  (StructRef2) = {
    val StructTemplata(env, structA) = structTemplata

    val fullName = env.fullName.addStep(translateStructNameWithTemplateArgs(structA.name, templateArgs))

    temputs.structDeclared(fullName) match {
      case Some(structRef2) => {
        (structRef2)
      }
      case None => {
        // not sure if this is okay or not, do we allow this?
        if (templateArgs.size != structA.identifyingRunes.size) {
          vfail("wat?")
        }
        val temporaryStructRef = StructRef2(fullName)
        temputs.declareStruct(temporaryStructRef)

        structA.maybePredictedMutability match {
          case None => temputs
          case Some(predictedMutability) => temputs.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(predictedMutability))
        }
        val (rulesForStructAndParents, typeByRuneForStructAndParents) =
//          EnvironmentUtils.assembleRulesFromEnvEntryAndParents(structAndParents)
          (structA.rules, structA.typeByRune)
        val result =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs,
            structA.identifyingRunes,
            rulesForStructAndParents,
            typeByRuneForStructAndParents,
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

        structA.maybePredictedMutability match {
          case None => temputs.declareStructMutability(temporaryStructRef, Conversions.evaluateMutability(structA.mutability))
          case Some(_) => temputs
        }

        StructTemplarMiddle.getStructRef(env, temputs, structA, inferences.templatasByRune)
      }
    }
  }

  def getInterfaceRef(
    temputs: TemputsBox,
    interfaceTemplata: InterfaceTemplata,
    templateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    val InterfaceTemplata(env, interfaceS) = interfaceTemplata
    val fullName = env.fullName.addStep(translateInterfaceNameWithTemplateArgs(interfaceS.name, templateArgs))

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
          case None =>
          case Some(predictedMutability) => temputs.declareInterfaceMutability(temporaryInterfaceRef, Conversions.evaluateMutability(predictedMutability))
        }

        val (rulesForInterfaceAndParents, typeByRuneForInterfaceAndParents) =
//          EnvironmentUtils.assembleRulesFromEnvEntryAndParents(interfaceAndParents)
          (interfaceS.rules, interfaceS.typeByRune)
        val result =
          InferTemplar.inferFromExplicitTemplateArgs(
            env,
            temputs,
            interfaceS.identifyingRunes,
            rulesForInterfaceAndParents,
            typeByRuneForInterfaceAndParents,
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
          case Some(_) =>
        }

        StructTemplarMiddle.getInterfaceRef(env, temputs, interfaceS, inferences.templatasByRune)
      }
    }
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: TemputsBox,
    name: LambdaNameA,
    functionS: FunctionA,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarMiddle.makeClosureUnderstruct(containingFunctionEnv, temputs, name, functionS, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnerstruct(env: NamespaceEnvironment[IName2], temputs: TemputsBox, memberTypes2: List[Coord], name: IStructName2):
  (StructRef2, Mutability) = {
    StructTemplarMiddle.makeSeqOrPackUnderstruct(env, temputs, memberTypes2, name)
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
    StructTemplarMiddle.makeAnonymousSubstruct(
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
    StructTemplarMiddle.prototypeToAnonymousSubstruct(
      outerEnv, temputs, interfaceRef, prototype)
  }
}
