package net.verdagon.vale.parser

import net.verdagon.vale.vassert

import scala.collection.immutable.List

sealed trait IVirtualityP
case object AbstractP extends IVirtualityP
case class OverrideP(tyype: ITemplexPPT) extends IVirtualityP

case class IntBox(var num: Int)

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
    val _ = capture

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
    state: State,
    templex: ITemplexPPT,
    handler: (Env, State, ITemplexPPT) => (ITemplexPPT)):
  (ITemplexPPT) = {
    templex match {
      case AnonymousRunePPT() => handler(env, state, templex)
      case IntPPT(value) => handler(env, state, templex)
      case BoolPPT(value) => handler(env, state, templex)
      case RunePPT(rune) => handler(env, state, templex)
      case NamePPT(name) => handler(env, state, templex)
      case MutabilityPPT(mutability) => handler(env, state, templex)
      case OwnershippedPPT(borrow, innerA) => {
        val innerB = traverseTemplex(env, state, innerA, handler)
        val newTemplex = OwnershippedPPT(borrow, innerB)
        handler(env, state, newTemplex)
      }
      case CallPPT(templateA, argsA) => {
        val templateB = traverseTemplex(env, state, templateA, handler)
        val argsB = traverseTemplexes(env, state, argsA, handler)
        val newTemplex = CallPPT(templateB, argsB)
        handler(env, state, newTemplex)
      }
      case RepeaterSequencePPT(mutabilityA, sizeA, elementA) => {
        val mutabilityB = traverseTemplex(env, state, mutabilityA, handler)
        val sizeB = traverseTemplex(env, state, sizeA, handler)
        val elementB = traverseTemplex(env, state, elementA, handler)
        val newTemplex = RepeaterSequencePPT(mutabilityB, sizeB, elementB)
        handler(env, state, newTemplex)
      }
      case ManualSequencePPT(membersA) => {
        val membersB = traverseTemplexes(env, state, membersA, handler)
        val newTemplex = ManualSequencePPT(membersB)
        handler(env, state, newTemplex)
      }
      case FunctionPPT(mutable, paramsA, retA) => {
        val paramsB = traverseTemplexes(env, state, paramsA, handler)
        val retB = traverseTemplex(env, state, retA, handler)
        val newTemplex = FunctionPPT(mutable, paramsB, retB)
        handler(env, state, newTemplex)
      }
      case PackPPT(membersA) => {
        val membersB = traverseTemplexes(env, state, membersA, handler)
        val newTemplex = PackPPT(membersB)
        handler(env, state, newTemplex)
      }
    }
  }

  def traverseTemplexes[Env, State](
    env: Env,
    state: State,
    templexes: List[ITemplexPPT],
    handler: (Env, State, ITemplexPPT) => (ITemplexPPT)):
  (List[ITemplexPPT]) = {
    templexes.foldLeft((List[ITemplexPPT]()))({
      case ((previousNewTemplexes), templex) => {
        val newTemplex = traverseTemplex[Env, State](env, state, templex, handler)
        (previousNewTemplexes :+ newTemplex)
      }
    })
  }
}
