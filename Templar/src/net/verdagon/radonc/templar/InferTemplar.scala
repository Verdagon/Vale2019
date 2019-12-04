package net.verdagon.radonc.templar

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.ITemplexS
import net.verdagon.radonc.templar.citizen.{ImplTemplar, StructTemplar}
import net.verdagon.radonc.templar.env.{IEnvironment, TemplataLookupContext}
import net.verdagon.radonc.templar.infer._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassertSome, vfail}

import scala.collection.immutable.List

object InferTemplar {
  private def solve(
    env: IEnvironment,
    state0: Temputs,
    rules0: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    directInputs: Map[String, ITemplata],
    paramAtoms: List[AtomSP],
    maybeParamInputs: Option[List[ParamFilter]],
    checkAllRunesPresent: Boolean,
  ): (Temputs, IInferSolveResult) = {
    Inferer.solve(
      makeEvaluateDelegate(),
      env,
      state0,
      rules0,
      typeByRune,
      directInputs,
      paramAtoms,
      maybeParamInputs,
      checkAllRunesPresent)
  }

  private def makeEvaluateDelegate(): IInfererDelegate[IEnvironment, Temputs] = {
    new IInfererDelegate[IEnvironment, Temputs] {
      override def evaluateType(
        env: IEnvironment,
        temputs0: Temputs,
        type1: ITemplexA
      ): (Temputs, ITemplata) = {
        TemplataTemplar.evaluateTemplex(env, temputs0, type1)
      }

      override def lookupMemberTypes(state0: Temputs, kind: Kind, expectedNumMembers: Int): Option[List[Coord]] = {
        val underlyingStructRef2 =
          kind match {
            case sr @ StructRef2(_) => sr
            case TupleT2(_, underlyingStruct) => underlyingStruct
            case PackT2(_, underlyingStruct) => underlyingStruct
            case _ => return None
          }
        val structDef2 = state0.lookupStruct(underlyingStructRef2)
        val structMemberTypes = structDef2.members.map(_.tyype.reference)
        Some(structMemberTypes)
      }

      override def getMutability(state0: Temputs, kind: Kind): Mutability = {
        Templar.getMutability(state0, kind)
      }

      override def lookupTemplata(env: IEnvironment, name: String): ITemplata = {
        // We can only ever lookup types by name in expression context,
        // otherwise we have no idea what List<Str> means; it could
        // mean a list of strings or a list of the Str(:Int)Str function.
        env.getNearestTemplataWithName(name, Set(TemplataLookupContext)) match {
          case None => vfail("Couldn't find anything with name: " + name)
          case Some(x) => x
        }
      }

      override def getPackKind(env: IEnvironment, state0: Temputs, members: List[Coord]): (Temputs, PackT2, Mutability) = {
        PackTemplar.makePackType(env.globalEnv, state0, members)
      }

      override def getArraySequenceKind(env: IEnvironment, state0: Temputs, mutability: Mutability, size: Int, element: Coord): (Temputs, ArraySequenceT2) = {
        ArrayTemplar.makeArraySequenceType(env, state0, mutability, size, element)
      }

      override def evaluateInterfaceTemplata(
        state0: Temputs,
        templata: InterfaceTemplata,
        templateArgs: List[ITemplata]):
      (Temputs, Kind) = {
        StructTemplar.getInterfaceRef(state0, templata, templateArgs)
      }

      override def evaluateStructTemplata(
        state0: Temputs,
        templata: StructTemplata,
        templateArgs: List[ITemplata]):
      (Temputs, Kind) = {
        StructTemplar.getStructRef(state0, templata, templateArgs)
      }

      override def getAncestorInterfaceDistance(temputs0: Temputs, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2):
      (Temputs, Option[Int]) = {
        ImplTemplar.getAncestorInterfaceDistance(temputs0, descendantCitizenRef, ancestorInterfaceRef)
      }

      override def getAncestorInterfaces(temputs0: Temputs, descendantCitizenRef: CitizenRef2): (Temputs, Set[InterfaceRef2]) = {
        ImplTemplar.getAncestorInterfaces(temputs0, descendantCitizenRef)
      }

      override def getMemberCoords(state: Temputs, structRef: StructRef2): List[Coord] = {
        StructTemplar.getMemberCoords(state, structRef)
      }

      override def citizenIsFromTemplate(state: Temputs, citizen: CitizenRef2, template: ITemplata): (Temputs, Boolean) = {
        StructTemplar.citizenIsFromTemplate(state, citizen, template)
      }


      override def getInterfaceTemplataType(it: InterfaceTemplata): ITemplataType = {
        it.originInterface.tyype
      }

      override def getStructTemplataType(st: StructTemplata): ITemplataType = {
        st.originStruct.tyype
      }
    }
  }

  // No incoming types needed (like manually specified template args, or argument coords from a call).
  // This is for when we want to figure out the types for an ordinary function like
  //   fn sum(a: Int, b: Int)Int { }
  // which, remember, actually *does* have rules:
  //   fn sum
  //   rules(#1 = Int, #2 = Int, #3 = Int)
  //   (a: #1, b: #2) #3 { ...}
  def inferOrdinaryRules(
    env0: IEnvironment,
    temputs0: Temputs,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType]
  ): (Temputs, Map[String, ITemplata]) = {
    solve(env0, temputs0, rules, typeByRune, Map(), List(), None, true) match {
      case (temputs1, InferSolveSuccess(inferences)) => {
        (temputs1, inferences.templatasByRune)
      }
      case (_, isf @ InferSolveFailure(_, _, _, _, _, _)) => {
        vfail("Conflict in determining ordinary rules' runes: " + isf)
      }
    }
  }

  def inferFromExplicitTemplateArgs(
    env0: IEnvironment,
    temputs0: Temputs,
    identifyingRunes: List[String],
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    patterns1: List[AtomSP],
    maybeRetRune: Option[String],
    explicits: List[ITemplata],
  ): (Temputs, IInferSolveResult) = {
    if (identifyingRunes.size != explicits.size) {
      vfail("Wrong number of template args!")
    }

    solve(
      env0,
      temputs0,
      rules,
      typeByRune,
      identifyingRunes.zip(explicits).toMap,
      patterns1,
      None,
      true)
  }

  def inferFromArgCoords(
    env0: IEnvironment,
    temputs0: Temputs,
    identifyingRunes: List[String],
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    patterns1: List[AtomSP],
    maybeRetRune: Option[String],
    alreadySpecifiedTemplateArgs: List[ITemplata],
    patternInputCoords: List[ParamFilter]
  ): (Temputs, IInferSolveResult) = {

    solve(
      env0,
      temputs0,
      rules,
      typeByRune,
      // Note: this two things we're zipping are of different length, that's fine.
      identifyingRunes.zip(alreadySpecifiedTemplateArgs).toMap,
      patterns1,
      Some(patternInputCoords),
      true)
  }
}
