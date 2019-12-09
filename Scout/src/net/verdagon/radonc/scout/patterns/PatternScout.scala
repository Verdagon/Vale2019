package net.verdagon.radonc.scout.patterns

import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object PatternScout {
  def getNextUnusedRune(allRunesSet: Set[String]): String = {
    // Guaranteed at least one of these is not in orderedRunes
    (0 until (allRunesSet.size + 1)).map("__" + _).filter(x => !allRunesSet.contains(x)).head
  }

  // A convenient packaging of these four different things so that PatternScout can
  // add to the rulexes, and can access maybeIdentifyingRules + patterns + maybeRetRune,
  // which are useful in calculating all the involved runes.
  case class InitialRulesAndRunes(
      userSpecifiedIdentifyingRunes: List[String],
      rulexes: List[IRulexPR],
      patterns: List[PatternPP],
      maybeRetTemplex: Option[ITemplexPPT]) {
  }

  // Sometimes referred to as a "rate"
  case class RuleState(rulexesS: List[IRulexSR]) {
    def allRunes(initial: InitialRulesAndRunes): List[String] = {
      // This takes into account both the runes from the initial rules, *and* the new rules.
      (
        initial.userSpecifiedIdentifyingRunes ++
          initial.patterns.flatMap(PatternPUtils.getOrderedRunesFromPatternWithDuplicates) ++
          initial.rulexes.flatMap(RulePUtils.getOrderedRunesFromRulexWithDuplicates) ++
          rulexesS.flatMap(RuleSUtils.getDistinctOrderedRunesForRulex) ++
          initial.maybeRetTemplex.map(PatternPUtils.getOrderedRunesFromTemplexWithDuplicates).getOrElse(List())
        ).distinct
    }

    def nextUnusedRune(initial: InitialRulesAndRunes): String = {
      getNextUnusedRune(allRunes(initial).toSet)
    }

    def newImplicitRune(initial: InitialRulesAndRunes, rune: String, tyype: ITypeSR): RuleState = {
      vassert(!allRunes(initial).contains(rune))
      addRule(TypedSR(Some(rune), tyype))
    }

    def addRule(rule: IRulexSR): RuleState = {
      RuleState(rulexesS :+ rule)
    }

    def addRules(rules: List[IRulexSR]): RuleState = {
      RuleState(rulexesS ++ rules)
    }
  }

  case class RuleStateBox(var rate: RuleState) {
    def allRunes(initial: InitialRulesAndRunes): List[String] = rate.allRunes(initial)

    def nextUnusedRune(initial: InitialRulesAndRunes): String = rate.nextUnusedRune(initial)

    def newImplicitRune(initial: InitialRulesAndRunes, rune: String, tyype: ITypeSR): Unit = {
      rate = rate.newImplicitRune(initial, rune, tyype)
    }

    def addRule(rule: IRulexSR): Unit = {
      rate = rate.addRule(rule)
    }

    def addRules(rules: List[IRulexSR]): Unit = {
      rate = rate.addRules(rules)
    }
  }


  def getParameterCaptures(pattern: AtomSP): Set[VariableDeclaration] = {
    val AtomSP(maybeCapture, _, _, maybeDestructure) = pattern
    Set() ++
        maybeCapture.toSet.flatMap(a => getCaptureCaptures(a)) ++
        maybeDestructure.toList.flatten.flatten.flatMap(getParameterCaptures)
  }
  private def getCaptureCaptures(capture: CaptureP): Set[VariableDeclaration] = {
    Set(VariableDeclaration(capture.name, capture.variability))
  }

  // Returns:
  // - New function state
  // - New rules
  // - Scouted pattern
  private[scout] def scoutPatterns(
      initialRulesAndRunes: InitialRulesAndRunes,
      fate: ScoutFateBox,
      rulesS: RuleStateBox,
      params: List[PatternPP],
      // If this is Some, then if there's no name for the given pattern, we'll give it this name.
      // probably "__l" for lets or "__p" for params
      maybeNameSuggestion: Option[String],
      // If this is Some, then if there's no rune for the given pattern, we'll give it this rune.
      // Probably None or "__Par"
      maybeRuneSuggestion: Option[String]):
  List[AtomSP] = {
    params.zipWithIndex.foldLeft(List[AtomSP]())({
      case (previousPatterns, (patternP, index)) => {
        val paramS =
          PatternScout.translatePattern(
            initialRulesAndRunes,
            fate,
            rulesS,
            patternP,
            maybeNameSuggestion.map(_ + index),
            maybeRuneSuggestion.map(_ + index))
        previousPatterns :+ paramS
      }
    })
  }
  sealed trait INameRequirement
  case object NameNotRequired extends INameRequirement
  case class NameRequired(nameSuggestion: String) extends INameRequirement

  private[scout] def translatePattern(
    initialRulesAndRunes: InitialRulesAndRunes,
    fate: ScoutFateBox,
    rulesS: RuleStateBox,
    patternPP: PatternPP,
    // If this is Some, then if there's no name for the given pattern, we'll give it this name.
    maybeNameSuggestion: Option[String],
    // If this is Some, then if there's no rune for the given pattern, we'll give it this rune.
    maybeRuneSuggestion: Option[String]):
  AtomSP = {
    val PatternPP(maybeCaptureP, maybeTypeP, maybeDestructureP, maybeVirtualityP) = patternPP

    val maybeVirtualityS =
      maybeVirtualityP match {
        case None => None
        case Some(AbstractP) => Some(AbstractSP)
        case Some(OverrideP(typeP)) => {
          val rune =
            translateMaybeTypeIntoRune(
              initialRulesAndRunes, rulesS, Some(typeP), KindTypePR, maybeRuneSuggestion.map(_ + Scout.unrunedParamOverrideRuneSuffix))
          Some(OverrideSP(rune))
        }
      }

    val maybeCaptureS =
      (maybeCaptureP, maybeNameSuggestion) match {
        case (Some(c), _) => Some(c)
        case (_, Some(nameSuggestion)) => Some(CaptureP(nameSuggestion, FinalP))
        case (None, None) => None
      }

    val coordRune =
      translateMaybeTypeIntoRune(
        initialRulesAndRunes, rulesS, maybeTypeP, CoordTypePR, maybeRuneSuggestion)

    val maybePatternMaybesS =
      maybeDestructureP match {
        case None => None
        case Some(destructureP) => {
          val patternMaybesS =
            destructureP.zipWithIndex.foldLeft(List[Option[AtomSP]]())({
              case (previousPatternMaybesS, (maybePatternP, index)) => {
                val maybePatternS =
                  maybePatternP match {
                    case None => None
                    case Some(patternP) => {
                      val patternS =
                        translatePattern(
                          initialRulesAndRunes,
                          fate,
                          rulesS,
                          patternP,
                          maybeRuneSuggestion.map(_ + Scout.unnamedMemberNameSeparator + index),
                          maybeRuneSuggestion.map(_ + Scout.memberRuneSeparator + index))
                      Some(patternS)
                    }
                  }
                (previousPatternMaybesS :+ maybePatternS)
              }
            })
          Some(patternMaybesS)
        }
      }

    val atomSP = AtomSP(maybeCaptureS, maybeVirtualityS, coordRune, maybePatternMaybesS)
    atomSP
  }

  def translateMaybeTypeIntoRune(
      initialRulesAndRunes: InitialRulesAndRunes,
      rulesS: RuleStateBox,
      maybeTypeP: Option[ITemplexPPT],
      runeType: ITypePR,
      maybeRuneSuggestion: Option[String]):
  String = {
    maybeTypeP match {
      case None => {
        val rune =
          maybeRuneSuggestion match {
            case None => rulesS.nextUnusedRune(initialRulesAndRunes)
            case Some(r) => r
          }
        rulesS.newImplicitRune(initialRulesAndRunes, rune, RuleScout.translateType(runeType))
        rune
      }
      case Some(RunePPT(rune)) => {
        vassert(rulesS.allRunes(initialRulesAndRunes).contains(rune))
        rulesS.addRule(TypedSR(Some(rune), RuleScout.translateType(runeType)))
        rune
      }
      case Some(nonRuneTemplexP) => {
        val (templexS, maybeRune) =
          translatePatternTemplex(initialRulesAndRunes, rulesS, nonRuneTemplexP, maybeRuneSuggestion)
        maybeRune match {
          case Some(rune) => rune
          case None => {
            // If it's none, then it definitely didn't take this suggestion, so use it now.
            val rune =
              maybeRuneSuggestion match {
                case None => rulesS.nextUnusedRune(initialRulesAndRunes)
                case Some(r) => r
              }
            rulesS.newImplicitRune(initialRulesAndRunes, rune, RuleScout.translateType(runeType))
            rulesS.addRule(EqualsSR(TemplexSR(RuneST(rune)), TemplexSR(templexS)))
            rune
          }
        }
      }
    }
  }
  def translateMaybeTypeIntoMaybeRune(
    initialRulesAndRunes: InitialRulesAndRunes,
    rulesS: RuleStateBox,
    maybeTypeP: Option[ITemplexPPT],
    runeType: ITypePR,
    maybeRuneSuggestion: Option[String]):
  Option[String] = {
    if (maybeTypeP.isEmpty) {
      None
    } else {
      val rune =
        translateMaybeTypeIntoRune(
          initialRulesAndRunes, rulesS, maybeTypeP, runeType, maybeRuneSuggestion)
      Some(rune)
    }
  }

//  private def translatePatternTemplexes(rulesS: WorkingRulesAndRunes, templexesP: List[ITemplexPPT]):
//  (List[IRulexSR], List[ITemplexS]) = {
//    templexesP match {
//      case Nil => (rulesS, Nil)
//      case headTemplexP :: tailTemplexesP => {
//        val (rulesS, headTemplexS) = translatePatternTemplex(rulesS, headTemplexP)
//        val (rulesS, tailTemplexesS) = translatePatternTemplexes(rulesS, tailTemplexesP)
//        (rulesS, headTemplexS :: tailTemplexesS)
//      }
//    }
//  }

  private def translatePatternTemplexes(initialRulesAndRunes: InitialRulesAndRunes, rulesS: RuleStateBox, templexesP: List[ITemplexPPT], maybeRuneSuggestion: Option[String]):
  (List[ITemplexS]) = {
    templexesP match {
      case Nil => Nil
      case headTemplexP :: tailTemplexesP => {
        val (headTemplexS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, headTemplexP, maybeRuneSuggestion)
        val tailTemplexesS = translatePatternTemplexes(initialRulesAndRunes, rulesS, tailTemplexesP, maybeRuneSuggestion)
        headTemplexS :: tailTemplexesS
      }
    }
  }

  private def translatePatternMaybeTemplex(
      initialRulesAndRunes: InitialRulesAndRunes,
      rulesS: RuleStateBox,
      maybeTemplexP: Option[ITemplexPPT],
      maybeRuneSuggestion: Option[String]):
  (Option[ITemplexS], Option[String]) = {
    maybeTemplexP match {
      case None => (None, None)
      case Some(templexP) => {
        val (templexS, maybeRune) =
          translatePatternTemplex(initialRulesAndRunes, rulesS, templexP, maybeRuneSuggestion)
        (Some(templexS), maybeRune)
      }
    }
  }

  // Returns:
  // - New rules
  // - A templex that represents the result
  // - If any, the rune associated with this exact result.
  def translatePatternTemplex(
      initialRulesAndRunes: InitialRulesAndRunes,
      rulesS: RuleStateBox,
      templexP: ITemplexPPT,
      maybeRuneSuggestion: Option[String]):
  (ITemplexS, Option[String]) = {
    templexP match {
      case IntPPT(value) => (IntST(value), None)
      case BoolPPT(value) => (BoolST(value), None)
      case RunePPT(rune) => (RuneST(rune), Some(rune))
      case NamePPT(name) => (NameST(name), None)
      case MutabilityPPT(mutability) => (MutabilityST(mutability), None)
      case OwnershippedPPT(BorrowP, RunePPT(ownedCoordRune)) => {
        // It's a user rune so it's already in the orderedRunes
        rulesS.addRule(TypedSR(Some(ownedCoordRune), CoordTypeSR))
        val kindRune =
          maybeRuneSuggestion.map(_ + "K") match {
            case None => rulesS.nextUnusedRune(initialRulesAndRunes)
            case Some(r) => r
          }
        rulesS.newImplicitRune(initialRulesAndRunes, kindRune, KindTypeSR)
        rulesS
            .addRule(
              ComponentsSR(
                TypedSR(Some(ownedCoordRune), CoordTypeSR),
                List(
                  TemplexSR(OwnershipST(OwnP)),
                  TemplexSR(RuneST(kindRune)))))
        val borrowedCoordRune =
          maybeRuneSuggestion match {
            case None => rulesS.nextUnusedRune(initialRulesAndRunes)
            case Some(r) => r
          }
        rulesS.newImplicitRune(initialRulesAndRunes, borrowedCoordRune, CoordTypeSR)
        rulesS
            .addRule(
              ComponentsSR(
                TypedSR(Some(borrowedCoordRune), CoordTypeSR),
                List(
                  TemplexSR(OwnershipST(BorrowP)),
                  TemplexSR(RuneST(kindRune)))))
        (RuneST(borrowedCoordRune), Some(borrowedCoordRune))
      }
      case OwnershippedPPT(ownership, innerP) => {
        val (innerS, _) =
          translatePatternTemplex(initialRulesAndRunes, rulesS, innerP, None)
        (OwnershippedST(ownership, innerS), None)
      }
      case CallPPT(maybeTemplateP, argsMaybeTemplexesP) => {
        val (maybeTemplateS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, maybeTemplateP, None)
        val argsMaybeTemplexesS = translatePatternTemplexes(initialRulesAndRunes, rulesS, argsMaybeTemplexesP, None)
        (CallST(maybeTemplateS, argsMaybeTemplexesS), None)
      }
      case RepeaterSequencePPT(mutabilityP, sizeP, elementP) => {
        val (mutabilityS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, mutabilityP, None)
        val (sizeS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, sizeP, None)
        val (elementS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, elementP, None)
        (RepeaterSequenceST(mutabilityS, sizeS, elementS), None)
      }
      case ManualSequencePPT(maybeMembersP) => {
        val maybeMembersS = translatePatternTemplexes(initialRulesAndRunes, rulesS, maybeMembersP, None)
        (ManualSequenceST(maybeMembersS), None)
      }
      case FunctionPPT(mutableP, paramsP, retP) => {
        val (mutableS, _) = translatePatternMaybeTemplex(initialRulesAndRunes, rulesS, mutableP, None)
        val paramsS = translatePatternTemplexes(initialRulesAndRunes, rulesS, paramsP, None)
        val (retS, _) = translatePatternTemplex(initialRulesAndRunes, rulesS, retP, None)

        vfail("impl!")
//        CallST(
//          NameST("IFunction"),
//          List(
//            mutableS.getOrElse(MutableP),
//            paramsS,
//            retS))

//        (rulesS, FunctionST(mutableS, PackST(paramsS), retS), None)
      }
      case PackPPT(membersP) => {
        val membersS = translatePatternTemplexes(initialRulesAndRunes, rulesS, membersP, None)
        (PackST(membersS), None)
      }
    }
  }
}
