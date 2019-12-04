package net.verdagon.radonc.scout.templatepredictor

import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.radonc.scout.predictor.{Conclusions, PredictorEvaluateResult}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.vfail

import scala.collection.immutable.List

// Given enough user specified template params and param inputs, we should be able to
// infer everything.
// This class's purpose is to take those things, and see if it can figure out as many
// inferences as possible.

object PredictorEvaluator {

  private[scout] def getAllRunes(
    identifyingRunes: List[String],
    rules: List[IRulexSR],
    patterns1: List[AtomSP],
    maybeRetRune: Option[String]
  ): Set[String] = {
    (
      identifyingRunes ++
        patterns1.flatMap(PatternSUtils.getDistinctOrderedRunesForPattern) ++
        RuleSUtils.getDistinctOrderedRunesForRulexes(rules) ++
        maybeRetRune.toList
      ).toSet
  }

  private[scout] def solve(
    rules0: List[IRulexSR],
    paramAtoms: List[AtomSP],
  ): Conclusions = {
    solveUntilSettled(rules0, Conclusions(Set(), Map()))
  }

  private def solveUntilSettled(
    rules: List[IRulexSR],
    conclusions0: Conclusions,
  ): Conclusions = {
    val PredictorEvaluateResult(conclusions10, _) = evaluateRules(conclusions0, rules)

    if (conclusions0 != conclusions10) {
      // Things have not settled, we made some sort of progress in this last iteration.
      // Keep going.
      solveUntilSettled(rules, conclusions10)
    } else {
      // No need to do one last match, because we just did an entire iteration where nothing changed.
      conclusions10
    }
  }

  private def evaluateRule(
    conclusions0: Conclusions,
    rule: IRulexSR,
  ): PredictorEvaluateResult[Boolean] = {
    rule match {
      case r @ EqualsSR(_, _) => evaluateEqualsRule(conclusions0, r)
      case r @ IsaSR(_, _) => evaluateIsaRule(conclusions0, r)
      case r @ OrSR(_) => evaluateOrRule(conclusions0, r)
      case r @ ComponentsSR(_, _) => evaluateComponentsRule(conclusions0, r)
      case r @ TypedSR(_, _) => evaluateTypedRule(conclusions0, r)
      case TemplexSR(templex) => evaluateTemplex(conclusions0, templex)
      case r @ CallSR(_, _) => evaluateRuleCall(conclusions0, r)
    }
  }

  private def evaluateRules(
    conclusions0: Conclusions,
    rules: List[IRulexSR],
  ): PredictorEvaluateResult[List[Boolean]] = {
    val initial = PredictorEvaluateResult[List[Boolean]](conclusions0, List())
    rules.foldLeft(initial)({
      case (PredictorEvaluateResult(conclusions1, previousKnowns), rule) => {
        val PredictorEvaluateResult(conclusions2, known) = evaluateRule(conclusions1, rule)
        PredictorEvaluateResult(conclusions2, previousKnowns :+ known)
      }
    })
  }

  private def evaluateRuleCall(
    conclusions0: Conclusions,
    ruleCall: CallSR,
  ): PredictorEvaluateResult[Boolean] = {
    val CallSR(name, argumentRules) = ruleCall

    name match {
      case "toRef" => {
        val List(kindRule) = argumentRules
        evaluateRule(conclusions0, kindRule)
      }
      case "passThroughIfConcrete" => {
        val List(kindRule) = argumentRules
        evaluateRule(conclusions0, kindRule)
      }
      case "passThroughIfStruct" => {
        val List(kindRule) = argumentRules
        evaluateRule(conclusions0, kindRule)
      }
      case "passThroughIfInterface" => {
        val List(kindRule) = argumentRules
        evaluateRule(conclusions0, kindRule)
      }
      case _ => vfail("Unknown function \"" + name + "\"!");
    }
  }
  private def evaluateTemplexes(
    conclusions0: Conclusions,
    ruleTemplexes: List[ITemplexS],
  ): PredictorEvaluateResult[List[Boolean]] = {
    val initial = List[Boolean]()
    val (conclusions10, knowns) =
      ruleTemplexes.foldLeft((conclusions0, initial))({
        case ((conclusions2, previous), ruleTemplex) => {
          val PredictorEvaluateResult(conclusions4, result) =
            evaluateTemplex(conclusions2, ruleTemplex)
          (conclusions4, previous :+ result)
        }
      })
    PredictorEvaluateResult(conclusions10, knowns)
  }

  private def evaluateTemplex(
    conclusions0: Conclusions,
    ruleTemplex: ITemplexS,
  ): PredictorEvaluateResult[Boolean] = {
    ruleTemplex match {
      case IntST(_) => PredictorEvaluateResult(conclusions0, true)
      case BoolST(_) => PredictorEvaluateResult(conclusions0, true)
      case MutabilityST(_) => PredictorEvaluateResult(conclusions0, true)
      case PermissionST(_) => PredictorEvaluateResult(conclusions0, true)
      case LocationST(_) => PredictorEvaluateResult(conclusions0, true)
      case OwnershipST(_) => PredictorEvaluateResult(conclusions0, true)
      case VariabilityST(_) => PredictorEvaluateResult(conclusions0, true)
      case NameST(_) => PredictorEvaluateResult(conclusions0, true)
      case AnonymousRuneST() => PredictorEvaluateResult(conclusions0, false)
      case RuneST(rune) => {
        PredictorEvaluateResult(conclusions0, conclusions0.knowableValueRunes.contains(rune))
      }
      case OwnershippedST(_, kindRule) => evaluateTemplex(conclusions0, kindRule)
      case CallST(templateRule, paramRules) => {
        val PredictorEvaluateResult(conclusions2, templateKnown) =
          evaluateTemplex(conclusions0, templateRule)
        val PredictorEvaluateResult(conclusions4, argsKnown) =
          evaluateTemplexes(conclusions2, paramRules)
        PredictorEvaluateResult(conclusions4, templateKnown && argsKnown.forall(_ == true))
      }
      case PrototypeST(_, _, _) => {
        vfail("Unimplemented")
      }
      case PackST(memberTemplexes) => {
        val PredictorEvaluateResult(conclusions4, membersKnown) =
          evaluateTemplexes(conclusions0, memberTemplexes)
        PredictorEvaluateResult(conclusions4, membersKnown.forall(_ == true))
      }
      case RepeaterSequenceST(mutabilityTemplex, sizeTemplex, elementTemplex) => {
        val PredictorEvaluateResult(conclusions1, mutabilityKnown) =
          evaluateTemplex(conclusions0, mutabilityTemplex)
        val PredictorEvaluateResult(conclusions2, sizeKnown) =
          evaluateTemplex(conclusions1, sizeTemplex)
        val PredictorEvaluateResult(conclusions4, elementKnown) =
          evaluateTemplex(conclusions2, elementTemplex)
        PredictorEvaluateResult(conclusions4, mutabilityKnown && sizeKnown && elementKnown)
      }
      case ManualSequenceST(elementsTemplexes) => {
        val PredictorEvaluateResult(conclusions4, membersKnown) =
          evaluateTemplexes(conclusions0, elementsTemplexes)
        PredictorEvaluateResult(conclusions4, membersKnown.forall(_ == true))
      }
    }
  }

  private def evaluateTypedRule(
    conclusions0: Conclusions,
    rule: TypedSR):
  PredictorEvaluateResult[Boolean] = {
    val TypedSR(maybeRune, tyype) = rule
    maybeRune match {
      case None => PredictorEvaluateResult(conclusions0, false)
      case Some(rune) => {
        val conclusions1 = conclusions0.markRuneTypeKnown(rune, tyype)
        PredictorEvaluateResult(
          conclusions1,
          conclusions1.knowableValueRunes.contains(rune))
      }
    }
  }

  private def evaluateEqualsRule(
    conclusions0: Conclusions,
    rule: EqualsSR,
  ): PredictorEvaluateResult[Boolean] = {
    val EqualsSR(leftRule, rightRule) = rule

    val PredictorEvaluateResult(conclusions1, leftKnown) =
      evaluateRule(conclusions0, leftRule)
    val PredictorEvaluateResult(conclusions2, rightKnown) =
      evaluateRule(conclusions1, rightRule)
    if (!leftKnown && !rightKnown) {
      PredictorEvaluateResult(conclusions2, false)
    } else {
      val conclusions10 =
        PredictorMatcher.matchAgainstRulexSR(conclusions2, leftRule)
      val conclusions20 =
        PredictorMatcher.matchAgainstRulexSR(conclusions10, rightRule)
      PredictorEvaluateResult(conclusions20, true)
    }
  }

  private def evaluateIsaRule(
    conclusions0: Conclusions,
    rule: IsaSR,
  ): PredictorEvaluateResult[Boolean] = {
    val IsaSR(leftRule, rightRule) = rule

    val PredictorEvaluateResult(conclusions1, leftKnown) =
      evaluateRule(conclusions0, leftRule)
    val PredictorEvaluateResult(conclusions2, rightKnown) =
      evaluateRule(conclusions1, rightRule)

    // Knowing the right rule doesn't really help us with anything, unfortunately...
    val (_) = rightKnown

    // We return the left thing for the rule, so if we know the left thing, we know the result of the rule.
    PredictorEvaluateResult(conclusions2, leftKnown)
  }

  private def evaluateOrRule(
    conclusions0: Conclusions,
    rule: OrSR
  ): PredictorEvaluateResult[Boolean] = {
    val PredictorEvaluateResult(conclusions1, possibilitiesKnowns) =
      evaluateRules(conclusions0, rule.alternatives)
    println("is this right?")
    // Just took a guess, really. Maybe we return true if one is known?
    PredictorEvaluateResult(conclusions1, possibilitiesKnowns.forall(_ == true))
  }

  private def evaluateComponentsRule(
    conclusions0: Conclusions,
    rule: ComponentsSR,
  ): PredictorEvaluateResult[Boolean] = {
    val ComponentsSR(typedRule, componentsRules) = rule

    val PredictorEvaluateResult(conclusions2, runeKnown) =
      evaluateRule(conclusions0, typedRule)

    val PredictorEvaluateResult(conclusions10, componentsKnown) =
      evaluateRules(conclusions2, componentsRules)
    val allComponentsKnown = componentsKnown.forall(_ == true)

    PredictorEvaluateResult(conclusions10, runeKnown || allComponentsKnown)
  }
}
