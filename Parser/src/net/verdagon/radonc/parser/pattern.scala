package net.verdagon.radonc.parser

import net.verdagon.radonc.vassert

import scala.collection.immutable.List

sealed trait IVirtualityP
case object AbstractP extends IVirtualityP
case class OverrideP(tyype: ITemplexPPT) extends IVirtualityP

case class PatternPP(
    capture: Option[CaptureP],

//    ownership: Option[OwnershipP],
//    // See RCKC for why we can't capture the kind rune.
//    coordRune: Option[String],
//    kind: Option[ITemplexPPT],

    // If they just have a destructure, this will probably be a ManualSequence(None).
    // If they have just parens, this will probably be a Pack(None).
    // Let's be careful to not allow destructuring packs without Pack here, see MEDP.
    templex: Option[ITemplexPPT],

    // Eventually, add an ellipsis: Boolean field here... except we also have
    // to account for the difference between a: T... and a...: T (in one, T is a
    // single type and in the other, T is a pack of types). And we might also want
    // to account for nested parens, like struct Fn:((#Params...), (#Rets...))

    destructure: Option[List[Option[PatternPP]]],
    virtuality: Option[IVirtualityP])

case class CaptureP(
    name: String,
    variability: VariabilityP)

sealed trait ITemplexPPT
case class IntPPT(value: Int) extends ITemplexPPT
case class BoolPPT(value: Boolean) extends ITemplexPPT
case class RunePPT(rune: String) extends ITemplexPPT
case class AnonymousRunePPT() extends ITemplexPPT
case class NamePPT(name: String) extends ITemplexPPT
case class MutabilityPPT(mutability: MutabilityP) extends ITemplexPPT
case class OwnershippedPPT(ownership: OwnershipP, inner: ITemplexPPT) extends ITemplexPPT
case class CallPPT(template: ITemplexPPT, args: List[ITemplexPPT]) extends ITemplexPPT
// We could phrase these all as ICallTemplexPPs but we want to be able to reconstruct
// a program from this AST.
case class RepeaterSequencePPT(mutability: ITemplexPPT, size: ITemplexPPT, element: ITemplexPPT) extends ITemplexPPT
case class ManualSequencePPT(members: List[ITemplexPPT]) extends ITemplexPPT
case class FunctionPPT(mutable: Option[ITemplexPPT], params: List[ITemplexPPT], ret: ITemplexPPT) extends ITemplexPPT
case class PackPPT(members: List[ITemplexPPT]) extends ITemplexPPT

object Patterns {
  def capture(name: String): PatternPP = {
    PatternPP(Some(CaptureP(name,FinalP)),None,None,None)
  }
  def withDestructure(atoms: PatternPP*) = {
    PatternPP(None,None,Some(atoms.map(a => Some(a)).toList),None)
  }
  def capturedWithType(name: String, templex: ITemplexPPT): PatternPP = {
    PatternPP(Some(CaptureP(name,FinalP)), Some(templex),None, None)
  }
  def capturedWithTypeRune(name: String, kindRune: String): PatternPP = {
    PatternPP(Some(CaptureP(name,FinalP)), Some(RunePPT(kindRune)), None, None)
  }
  def withType(kind: ITemplexPPT): PatternPP = {
    PatternPP(None, Some(kind), None, None)
  }
  def withTypeRune(rune: String): PatternPP = {
    PatternPP(None, Some(RunePPT(rune)), None, None)
  }
  def fromEnv(kindName: String): PatternPP = {
    PatternPP(None, Some(NamePPT(kindName)), None, None)
  }
}

object PatternPUtils {
  def getOrderedIdentifyingRunesFromPattern(atom: PatternPP): List[String] = {
    val PatternPP(capture, templex, virtuality, destructures) = atom

    // We don't care about capture, it can have no runes.
    val (_) = capture

    // No identifying runes come from overrides, see NIPFO.

    // No identifying runes come from destructures, see DCSIR.

    // So we just care about identifying runes from the templex.
    // Note, this assumes that we've filled the pattern (it's present, no anonymous runes anywhere in it).
    vassert(templex.nonEmpty)
    templex.toList.flatMap(getOrderedRunesFromTemplexWithDuplicates).distinct
  }

  def getOrderedRunesFromPatternWithDuplicates(atom: PatternPP): List[String] = {
    val destructures = atom.destructure.toList.flatten.flatten

    atom.virtuality.toList.flatMap(getOrderedRunesFromVirtualityWithDuplicates) ++
    atom.templex.toList.flatMap(getOrderedRunesFromTemplexWithDuplicates) ++
    destructures.flatMap(getOrderedRunesFromPatternWithDuplicates)
  }

  private def getOrderedRunesFromVirtualityWithDuplicates(virtuality: IVirtualityP): List[String] = {
    virtuality match {
      case AbstractP => List()
      case OverrideP(tyype: ITemplexPPT) => getOrderedRunesFromTemplexWithDuplicates(tyype)
    }
  }
  private def getOrderedRunesFromTemplexesWithDuplicates(templexes: List[ITemplexPPT]): List[String] = {
    templexes.foldLeft(List[String]())({
      case (previous, current) => previous ++ getOrderedRunesFromTemplexWithDuplicates(current)
    })
  }

  def getOrderedRunesFromTemplexWithDuplicates(templex: ITemplexPPT): List[String] = {
    templex match {
      case IntPPT(value) => List()
      case BoolPPT(value) => List()
      case RunePPT(rune) => List(rune)
      case NamePPT(name) => List()
      case MutabilityPPT(_) => List()
      case OwnershippedPPT(_, inner) => getOrderedRunesFromTemplexWithDuplicates(inner)
      case CallPPT(template, args) => getOrderedRunesFromTemplexesWithDuplicates((template :: args))
      case RepeaterSequencePPT(mutability, size, element) => getOrderedRunesFromTemplexesWithDuplicates(List(mutability, size, element))
      case ManualSequencePPT(members) => getOrderedRunesFromTemplexesWithDuplicates(members)
      case FunctionPPT(mutable, params, ret) => getOrderedRunesFromTemplexesWithDuplicates(params :+ ret)
      case PackPPT(members) => getOrderedRunesFromTemplexesWithDuplicates(members)
    }
  }

  def traverseTemplex[Env, State](
    env: Env,
    state0: State,
    templex: ITemplexPPT,
    handler: (Env, State, ITemplexPPT) => (State, ITemplexPPT)):
  (State, ITemplexPPT) = {
    templex match {
      case AnonymousRunePPT() => handler(env, state0, templex)
      case IntPPT(value) => handler(env, state0, templex)
      case BoolPPT(value) => handler(env, state0, templex)
      case RunePPT(rune) => handler(env, state0, templex)
      case NamePPT(name) => handler(env, state0, templex)
      case MutabilityPPT(mutability) => handler(env, state0, templex)
      case OwnershippedPPT(borrow, innerA) => {
        val (state1, innerB) = traverseTemplex(env, state0, innerA, handler)
        val newTemplex = OwnershippedPPT(borrow, innerB)
        handler(env, state1, newTemplex)
      }
      case CallPPT(templateA, argsA) => {
        val (state1, templateB) = traverseTemplex(env, state0, templateA, handler)
        val (state2, argsB) = traverseTemplexes(env, state1, argsA, handler)
        val newTemplex = CallPPT(templateB, argsB)
        handler(env, state2, newTemplex)
      }
      case RepeaterSequencePPT(mutabilityA, sizeA, elementA) => {
        val (state1, mutabilityB) = traverseTemplex(env, state0, mutabilityA, handler)
        val (state2, sizeB) = traverseTemplex(env, state1, sizeA, handler)
        val (state3, elementB) = traverseTemplex(env, state2, elementA, handler)
        val newTemplex = RepeaterSequencePPT(mutabilityB, sizeB, elementB)
        handler(env, state3, newTemplex)
      }
      case ManualSequencePPT(membersA) => {
        val (state1, membersB) = traverseTemplexes(env, state0, membersA, handler)
        val newTemplex = ManualSequencePPT(membersB)
        handler(env, state1, newTemplex)
      }
      case FunctionPPT(mutable, paramsA, retA) => {
        val (state1, paramsB) = traverseTemplexes(env, state0, paramsA, handler)
        val (state2, retB) = traverseTemplex(env, state1, retA, handler)
        val newTemplex = FunctionPPT(mutable, paramsB, retB)
        handler(env, state2, newTemplex)
      }
      case PackPPT(membersA) => {
        val (state1, membersB) = traverseTemplexes(env, state0, membersA, handler)
        val newTemplex = PackPPT(membersB)
        handler(env, state1, newTemplex)
      }
    }
  }

  def traverseTemplexes[Env, State](
    env: Env,
    state0: State,
    templexes: List[ITemplexPPT],
    handler: (Env, State, ITemplexPPT) => (State, ITemplexPPT)):
  (State, List[ITemplexPPT]) = {
    templexes.foldLeft((state0, List[ITemplexPPT]()))({
      case ((state1, previousNewTemplexes), templex) => {
        val (state2, newTemplex) =
          traverseTemplex[Env, State](env, state1, templex, handler)
        (state2, previousNewTemplexes :+ newTemplex)
      }
    })
  }
}
