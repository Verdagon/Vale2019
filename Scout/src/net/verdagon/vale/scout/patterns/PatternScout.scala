package net.verdagon.vale.scout.patterns

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.{vassert, vassertSome, vfail}

import scala.collection.immutable.List

// Sometimes referred to as a "rate"
case class RuleState(
  envFullName: AbsoluteNameS[INameS],
  nextImplicitRune: Int) {
  def newImplicitRune(): (RuleState, AbsoluteNameS[IRuneS]) = {
    (RuleState(envFullName, nextImplicitRune + 1), envFullName.addStep(ImplicitRuneS(nextImplicitRune)))
  }
}

case class RuleStateBox(var rate: RuleState) {
  def newImplicitRune(): AbsoluteNameS[IRuneS] = {
    val (newRate, rune) = rate.newImplicitRune()
    rate = newRate
    rune
  }
}

object PatternScout {
  def getParameterCaptures(pattern: AtomSP): Set[VariableDeclaration] = {
    val AtomSP(maybeCapture, _, _, maybeDestructure) = pattern
    Set() ++
        getCaptureCaptures(maybeCapture) ++
        maybeDestructure.toList.flatten.flatMap(getParameterCaptures)
  }
  private def getCaptureCaptures(capture: CaptureS): Set[VariableDeclaration] = {
    Set(VariableDeclaration(capture.name, capture.variability))
  }

  // Returns:
  // - New rules
  // - Scouted patterns
  private[scout] def scoutPatterns(
      stackFrame: StackFrame,
      fate: ScoutFateBox,
      rulesS: RuleStateBox,
      params: List[PatternPP]):
  (List[IRulexSR], List[AtomSP]) = {
    params.foldLeft((List[IRulexSR](), List[AtomSP]()))({
      case ((previousNewRulesS, previousPatternsS), patternP) => {
        val (newRulesS, patternS) =
          PatternScout.translatePattern(stackFrame, fate, rulesS, patternP)
        (previousNewRulesS ++ newRulesS, previousPatternsS :+ patternS)
      }
    })
  }

  sealed trait INameRequirement
  case object NameNotRequired extends INameRequirement
  case class NameRequired(nameSuggestion: String) extends INameRequirement

  // Returns:
  // - Rules, which are likely just TypedSR
  // - The translated patterns
  private[scout] def translatePattern(
    stackFrame: StackFrame,
    fate: ScoutFateBox,
    ruleState: RuleStateBox,
    patternPP: PatternPP):
  (List[IRulexSR], AtomSP) = {
    val PatternPP(maybeCaptureP, maybeTypeP, maybeDestructureP, maybeVirtualityP) = patternPP

    val declaredRunes = stackFrame.parentEnv.allUserDeclaredRunes()

    val (newRulesFromVirtuality, maybeVirtualityS) =
      maybeVirtualityP match {
        case None => (List(), None)
        case Some(AbstractP) => (List(), Some(AbstractSP))
        case Some(OverrideP(typeP)) => {
          val (newRulesFromVirtuality, rune) =
            translateMaybeTypeIntoRune(
              declaredRunes,
              ruleState, Some(typeP), KindTypePR)
          (newRulesFromVirtuality, Some(OverrideSP(rune)))
        }
      }

    val (newRulesFromType, coordRune) =
      translateMaybeTypeIntoRune(
        declaredRunes, ruleState, maybeTypeP, CoordTypePR)

    val (newRulesFromDestructures, maybePatternsS) =
      maybeDestructureP match {
        case None => (List(), None)
        case Some(destructureP) => {
          val (newRulesFromDestructures, patternsS) =
            destructureP.foldLeft((List[IRulexSR](), List[AtomSP]()))({
              case ((previousNewRulesS, previousPatternsS), patternP) => {
                val (newRulesFromDestructure, patternS) =
                  translatePattern(stackFrame, fate, ruleState, patternP)
                (previousNewRulesS ++ newRulesFromDestructure, previousPatternsS :+ patternS)
              }
            })
          (newRulesFromDestructures, Some(patternsS))
        }
      }

    val captureS =
      maybeCaptureP match {
        case None => {
          val codeLocation = CodeLocationS(patternPP.pos.line, patternPP.pos.column)
          CaptureS(stackFrame.name.addStep(UnnamedLocalNameS(codeLocation)), FinalP)
        }
        case Some(CaptureP(name, variability)) => {
          CaptureS(stackFrame.name.addStep(CodeVarNameS(name)), variability)
        }
      }

    val atomSP = AtomSP(captureS, maybeVirtualityS, coordRune, maybePatternsS)
    (newRulesFromType ++ newRulesFromDestructures ++ newRulesFromVirtuality, atomSP)
  }

  def translateMaybeTypeIntoRune(
      declaredRunes: Set[AbsoluteNameS[IRuneS]],
      rulesS: RuleStateBox,
      maybeTypeP: Option[ITemplexPPT],
      runeType: ITypePR):
  (List[IRulexSR], AbsoluteNameS[IRuneS]) = {
    maybeTypeP match {
      case None => {
        val rune = rulesS.newImplicitRune()
        val newRule = TypedSR(rune, RuleScout.translateType(runeType))
        (List(newRule), rune)
      }
      case Some(NameOrRunePPT(nameOrRune)) if declaredRunes.exists(_.last == CodeRuneS(nameOrRune)) => {
        val rune = vassertSome(declaredRunes.find(_.last == CodeRuneS(nameOrRune)))
        val newRule = TypedSR(rune, RuleScout.translateType(runeType))
        (List(newRule), rune)
      }
      case Some(nonRuneTemplexP) => {
        val (newRulesFromInner, templexS, maybeRune) =
          translatePatternTemplex(declaredRunes, rulesS, nonRuneTemplexP)
        maybeRune match {
          case Some(rune) => (newRulesFromInner, rune)
          case None => {
            val rune = rulesS.newImplicitRune()
            val newRule = EqualsSR(TypedSR(rune, RuleScout.translateType(runeType)), TemplexSR(templexS))
            (newRulesFromInner ++ List(newRule), rune)
          }
        }
      }
    }
  }
  def translateMaybeTypeIntoMaybeRune(
    declaredRunes: Set[AbsoluteNameS[IRuneS]],
    rulesS: RuleStateBox,
    maybeTypeP: Option[ITemplexPPT],
    runeType: ITypePR):
  (List[IRulexSR], Option[AbsoluteNameS[IRuneS]]) = {
    if (maybeTypeP.isEmpty) {
      (List(), None)
    } else {
      val (newRules, rune) =
        translateMaybeTypeIntoRune(
          declaredRunes, rulesS, maybeTypeP, runeType)
      (newRules, Some(rune))
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

  private def translatePatternTemplexes(
    declaredRunes: Set[AbsoluteNameS[IRuneS]],
    rulesS: RuleStateBox,
    templexesP: List[ITemplexPPT]):
  (List[IRulexSR], List[ITemplexS]) = {
    templexesP match {
      case Nil => (Nil, Nil)
      case headTemplexP :: tailTemplexesP => {
        val (newRulesFromHead, headTemplexS, _) = translatePatternTemplex(declaredRunes, rulesS, headTemplexP)
        val (newRulesFromTail, tailTemplexesS) = translatePatternTemplexes(declaredRunes, rulesS, tailTemplexesP)
        (newRulesFromHead ++ newRulesFromTail, headTemplexS :: tailTemplexesS)
      }
    }
  }

  // Returns:
  // - Any new rules we need to add
  // - A templex that represents the result
  // - If any, the rune associated with this exact result.
  def translatePatternTemplex(
      declaredRunes: Set[AbsoluteNameS[IRuneS]],
      rulesS: RuleStateBox,
      templexP: ITemplexPPT):
  (List[IRulexSR], ITemplexS, Option[AbsoluteNameS[IRuneS]]) = {
    templexP match {
      case IntPPT(value) => (List(), IntST(value), None)
      case BoolPPT(value) => (List(), BoolST(value), None)
      case NameOrRunePPT(nameOrRune) => {
        declaredRunes.find(_.last == CodeRuneS(nameOrRune)) match {
          case Some(foundRune) => (List(), RuneST(foundRune), Some(foundRune))
          case None => (List(), NameST(ImpreciseNameS(List(), CodeTypeNameS(nameOrRune))), None)
        }
      }
      case MutabilityPPT(mutability) => (List(), MutabilityST(mutability), None)
      case OwnershippedPPT(BorrowP, NameOrRunePPT(ownedCoordRuneName)) if declaredRunes.exists(_.last == CodeRuneS(ownedCoordRuneName)) => {
        val ownedCoordRune = vassertSome(declaredRunes.find(_.last == CodeRuneS(ownedCoordRuneName)))
        val kindRune = rulesS.newImplicitRune()
        val borrowedCoordRune = rulesS.newImplicitRune()
        val newRules =
          List(
            TypedSR(kindRune, KindTypeSR),
            // It's a user rune so it's already in the orderedRunes
            TypedSR(ownedCoordRune, CoordTypeSR),
            TypedSR(borrowedCoordRune, CoordTypeSR),
            ComponentsSR(
              TypedSR(ownedCoordRune, CoordTypeSR),
              List(
                TemplexSR(OwnershipST(OwnP)),
                TemplexSR(RuneST(kindRune)))),
            ComponentsSR(
              TypedSR(borrowedCoordRune, CoordTypeSR),
              List(
                TemplexSR(OwnershipST(BorrowP)),
                TemplexSR(RuneST(kindRune)))))
        (newRules, RuneST(borrowedCoordRune), Some(borrowedCoordRune))
      }
      case OwnershippedPPT(ownership, innerP) => {
        val (newRules, innerS, _) =
          translatePatternTemplex(declaredRunes, rulesS, innerP)
        (newRules, OwnershippedST(ownership, innerS), None)
      }
      case CallPPT(maybeTemplateP, argsMaybeTemplexesP) => {
        val (newRulesFromTemplate, maybeTemplateS, _) = translatePatternTemplex(declaredRunes, rulesS, maybeTemplateP)
        val (newRulesFromArgs, argsMaybeTemplexesS) = translatePatternTemplexes(declaredRunes, rulesS, argsMaybeTemplexesP)
        (newRulesFromTemplate ++ newRulesFromArgs, CallST(maybeTemplateS, argsMaybeTemplexesS), None)
      }
      case RepeaterSequencePPT(mutabilityP, sizeP, elementP) => {
        val (newRulesFromMutability, mutabilityS, _) = translatePatternTemplex(declaredRunes, rulesS, mutabilityP)
        val (newRulesFromSize, sizeS, _) = translatePatternTemplex(declaredRunes, rulesS, sizeP)
        val (newRulesFromElement, elementS, _) = translatePatternTemplex(declaredRunes, rulesS, elementP)
        (newRulesFromMutability ++ newRulesFromSize ++ newRulesFromElement, RepeaterSequenceST(mutabilityS, sizeS, elementS), None)
      }
      case ManualSequencePPT(maybeMembersP) => {
        val (newRules, maybeMembersS) = translatePatternTemplexes(declaredRunes, rulesS, maybeMembersP)
        (newRules, ManualSequenceST(maybeMembersS), None)
      }
      case FunctionPPT(mutableP, paramsP, retP) => {
//        val (mutableS, _) = translatePatternMaybeTemplex(declaredRunes, rulesS, mutableP, None)
//        val paramsS = translatePatternTemplexes(declaredRunes, rulesS, paramsP)
//        val (retS, _) = translatePatternTemplex(declaredRunes, rulesS, retP)

        vfail("impl!")
//        CallST(
//          NameST("IFunction"),
//          List(
//            mutableS.getOrElse(MutableP),
//            paramsS,
//            retS))

//        (rulesS, FunctionST(mutableS, PackST(paramsS), retS), None)
      }
    }
  }
}
