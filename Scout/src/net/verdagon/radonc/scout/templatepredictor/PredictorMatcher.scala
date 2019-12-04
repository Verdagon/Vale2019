package net.verdagon.radonc.scout.templatepredictor

import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.predictor.Conclusions
import net.verdagon.radonc.scout.rules._

object PredictorMatcher {
  def matchAgainstTemplexesSR(
    conclusions0: Conclusions,
    templexes: List[ITemplexS]):
  Conclusions = {
    templexes.foldLeft(conclusions0)({
      case (conclusions1, templex) => matchAgainstTemplexSR(conclusions1, templex)
    })
  }

  def matchAgainstTemplexSR(
      conclusions0: Conclusions,
      rule: ITemplexS):
  Conclusions = {
    rule match {
      case AnonymousRuneST() => conclusions0
      case IntST(_) => conclusions0
      case BoolST(_) => conclusions0
      case MutabilityST(_) => conclusions0
      case PermissionST(_) => conclusions0
      case LocationST(_) => conclusions0
      case OwnershipST(_) => conclusions0
      case VariabilityST(_) => conclusions0
      case NameST(_) => conclusions0
      case RuneST(rune) => conclusions0.markRuneValueKnowable(rune)
      case CallST(template, args) => {
        val conclusions1 = matchAgainstTemplexSR(conclusions0, template)
        val conclusions2 = matchAgainstTemplexesSR(conclusions1, args)
        conclusions2
      }
      case OwnershippedST(_, inner) => matchAgainstTemplexSR(conclusions0, inner)
      case RepeaterSequenceST(mutabilityRule, sizeRule,elementRule) => {
        val conclusions1 = matchAgainstTemplexSR(conclusions0, mutabilityRule)
        val conclusions2 = matchAgainstTemplexSR(conclusions1, sizeRule)
        val conclusions3 = matchAgainstTemplexSR(conclusions2, elementRule)
        conclusions3
      }
      case _ => vimpl()
    }
  }

  def matchAgainstRulexesSR(
    conclusions0: Conclusions,
    rules: List[IRulexSR]):
  Conclusions = {
    rules.foldLeft(conclusions0)({
      case (conclusions1, rule) => matchAgainstRulexSR(conclusions1, rule)
    })
  }

  def matchAgainstRulexSR(
    
    conclusions0: Conclusions,
    irule: IRulexSR):
  Conclusions = {
    irule match {
      case rule @ EqualsSR(_, _) => matchAgainstEqualsSR(conclusions0, rule)
      case rule @ OrSR(_) => matchAgainstOrSR(conclusions0, rule)
      case rule @ ComponentsSR(_, _) => matchAgainstComponentsSR(conclusions0, rule)
      case rule @ TypedSR(_, _) => matchAgainstTypedSR(conclusions0, rule)
      case TemplexSR(itemplexST) => matchAgainstTemplexSR(conclusions0, itemplexST)
      case rule @ CallSR(_, _) => matchAgainstCallSR(conclusions0, rule)
    }
  }

  def matchAgainstTypedSR(
    
    conclusions0: Conclusions,
    rule: TypedSR):
  Conclusions = {
    val TypedSR(maybeRune, _) = rule
    maybeRune match {
      case None => conclusions0
      case Some(rune) => conclusions0.markRuneValueKnowable(rune)
    }
  }

  def matchAgainstCallSR(
    conclusions0: Conclusions,
    rule: CallSR):
  Conclusions = {
    val CallSR(_, argRules) = rule

    // We don't do anything with the argRules; we don't evaluate or match them here, see MDMIA.
    val _ = argRules

    // We could check that the types are good, but we already do that in the evaluate layer.
    // So... nothing to do here!
    (conclusions0)
  }

  def matchAgainstComponentsSR(
    conclusions0: Conclusions,
    rule: ComponentsSR):
  Conclusions = {
    val ComponentsSR(container, components) = rule
    val conclusions2 = matchAgainstTypedSR(conclusions0, container)
    val conclusions4 = matchAgainstRulexesSR(conclusions2, components)
    conclusions4
  }

  def matchAgainstEqualsSR(
    conclusions0: Conclusions,
    rule: EqualsSR):
  Conclusions = {
    val EqualsSR(left, right) = rule
    val conclusions1 = matchAgainstRulexSR(conclusions0, left)
    val conclusions2 = matchAgainstRulexSR(conclusions1, right)
    conclusions2
  }

  def matchAgainstOrSR(
    conclusions0: Conclusions,
    rule: OrSR):
  Conclusions = {
    // Do nothing... information doesn't flow downwards into Ors
    conclusions0
  }
}
