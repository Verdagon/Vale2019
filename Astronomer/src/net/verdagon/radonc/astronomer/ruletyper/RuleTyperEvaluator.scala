package net.verdagon.radonc.astronomer.ruletyper

import net.verdagon.radonc.astronomer.ITemplataType
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.vfail
import net.verdagon.radonc.astronomer._

import scala.collection.immutable.List

trait IRuleTyperEvaluatorDelegate[Env, State] {
  def lookupType(state: State, env: Env, name: String): (State, ITemplataType)
}


// Given enough user specified template params and param inputs, we should be able to
// infer everything.
// This class's purpose is to take those things, and see if it can figure out as many
// inferences as possible.

class RuleTyperEvaluator[Env, State](
  delegate: IRuleTyperEvaluatorDelegate[Env, State]) {

  def solve(
    state0: State,
    env: Env,
    rules0: List[IRulexSR],
    paramAtoms: List[AtomSP],
    maybeNeededRunes: Option[Set[String]]
  ): (State, IRuleTyperSolveResult[List[IRulexAR]]) = {
    // First, we feed into the system the things the user already specified.

    // This used to be a parameter for some reason... could move it back if needed.
    val conclusions0 = Conclusions(Map())

    // Now we'll try solving a bunch, just to see if there's any contradictions,
    // and if so bail out early.
    val (state15, conclusions15) =
      solveUntilSettled(state0, env, rules0, conclusions0) match {
        case (state11, isc @ RuleTyperSolveFailure(_, _, _)) => return (state11, RuleTyperSolveFailure(conclusions0, "Failed during pre-solving!", List(isc)))
        case (state11, RuleTyperSolveSuccess(conclusions11, _)) => (state11, conclusions11)
      }

    // Now we have template args the user specified, and we know there's no contradictions yet.

    // Next, we'll feed in the arguments that they used in the call.

    val (state20, conclusions20) =
      paramAtoms.zipWithIndex.foldLeft((state15, conclusions15))({
        case ((state16, conclusions16), (paramAtom, paramIndex)) => {
          makeMatcher().matchAgainstAtomSP(state16, env, conclusions15, paramAtom) match {
            case (state18, isc @ RuleTyperMatchConflict(_, _, _)) => return (state18, RuleTyperSolveFailure(conclusions16, "Failed solving types for param " + paramIndex, List(isc)))
            case (state18, RuleTyperMatchSuccess(conclusions18, ())) => (state18, conclusions18)
          }
        }
      })

    val (state30, conclusions30, listOfMaybeRuleTypes) =
      solveUntilSettled(state20, env, rules0, conclusions20) match {
        case (state23, isc @ RuleTyperSolveFailure(_, _, _)) => return (state23, RuleTyperSolveFailure(conclusions20, "Failed to solve!", List(isc)))
        case (state23, RuleTyperSolveSuccess(c, rt)) => (state23, c, rt)
      }

    // No need to do one last match, because we just did an entire iteration where nothing changed.

    val knowns = listOfMaybeRuleTypes.collect({ case Some(x) => x })
    if (knowns.size != listOfMaybeRuleTypes.size) {
      val unknownIndices =
        listOfMaybeRuleTypes.zipWithIndex.filter(_._1.isEmpty).map(_._2)
      return (
        state30,
        RuleTyperSolveFailure(
          conclusions30,
          "Couldn't figure out types of all rules! Couldn't figure out rules at indices: " + unknownIndices,
          List()))
    }

    val unfiguredOutRunes = maybeNeededRunes.getOrElse(Set()) -- conclusions30.typeByRune.keySet
    if (unfiguredOutRunes.nonEmpty) {
      return (
        state30,
        RuleTyperSolveFailure(
          conclusions30,
          "Couldn't figure out types of all runes! Couldn't figure out: " + unfiguredOutRunes,
          List()))
    }

    (state30, RuleTyperSolveSuccess(conclusions30, knowns))
  }

  private def solveUntilSettled(
    state0: State,
    env: Env,
    rules: List[IRulexSR],
    conclusions0: Conclusions,
  ): (State, IRuleTyperSolveResult[List[Option[IRulexAR]]]) = {
    val initial: List[Option[IRulexAR]] = List()
    val (state10, conclusions10, results) =
      rules.foldLeft((state0, conclusions0, initial))({
        case ((state1, conclusions1, previousResults), rule) => {
          evaluateRule(state1, env, conclusions1, rule) match {
            case (state2, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state2, RuleTyperSolveFailure(conclusions1, "", List(iec)))
            case (state2, RuleTyperEvaluateUnknown(conclusions2)) => (state2, conclusions2, previousResults :+ None)
            case (state2, RuleTyperEvaluateSuccess(conclusions2, result)) => (state2, conclusions2, previousResults :+ Some(result))
          }
        }
      })

    if (conclusions0 != conclusions10) {
      // Things have not settled, we made some sort of progress in this last iteration.
      // Keep going.
      solveUntilSettled(state10, env, rules, conclusions10)
    } else {
      (state10, RuleTyperSolveSuccess(conclusions10, results))
    }
  }

  def evaluateRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: IRulexSR,
  ): (State, IRuleTyperEvaluateResult[IRulexAR]) = {
    rule match {
      case r @ IsaSR(_, _) => evaluateIsaRule(state0, env, conclusions0, r)
      case r @ EqualsSR(_, _) => evaluateEqualsRule(state0, env, conclusions0, r)
      case r @ OrSR(_) => evaluateOrRule(state0, env, conclusions0, r)
      case r @ ComponentsSR(_, _) => evaluateComponentsRule(state0, env, conclusions0, r)
      case r @ TypedSR(_, _) => evaluateTypedRule(state0, env, conclusions0, r)
      case TemplexSR(templexS) => {
        evaluateTemplex(state0, env, conclusions0, templexS) match {
          case (state1, rtec @ RuleTyperEvaluateConflict(_, _, _)) => (state1, RuleTyperEvaluateConflict(conclusions0, "", Some(rtec)))
          case (state1, RuleTyperEvaluateUnknown(conclusions1)) => (state1, RuleTyperEvaluateUnknown(conclusions1))
          case (state1, RuleTyperEvaluateSuccess(conclusions1, templexT)) => {
            (state1, RuleTyperEvaluateSuccess(conclusions1, TemplexAR(templexT)))
          }
        }
      }
      case r @ CallSR(_, _) => evaluateRuleCall(state0, env, conclusions0, r)
      case _ => vfail()
    }
  }

  def evaluateRules(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rules: List[IRulexSR],
  ): (State, IRuleTyperEvaluateResult[List[IRulexAR]]) = {
    val initialResult: IRuleTyperEvaluateResult[List[IRulexAR]] =
      RuleTyperEvaluateSuccess(conclusions0, List())
    rules.zipWithIndex.foldLeft((state0, initialResult))({
      case ((state1, RuleTyperEvaluateUnknown(conclusions6)), (rule, index)) => {
        evaluateRule(state1, env, conclusions6, rule) match {
          case (state2, iec @ RuleTyperEvaluateConflict(_, _, _)) => {
            return (state2, RuleTyperEvaluateConflict(conclusions6, "Failed evaluating rule index " + index, Some(iec)))
          }
          case (state2, RuleTyperEvaluateUnknown(conclusions7)) => {
            (state2, RuleTyperEvaluateUnknown(conclusions7))
          }
          case (state2, RuleTyperEvaluateSuccess(conclusions7, result)) => {
            // Throw it away; since one is unknown theyre all unknown
            val (_) = result
            (state2, RuleTyperEvaluateUnknown(conclusions7))
          }
        }
      }
      case ((state1, RuleTyperEvaluateSuccess(conclusions6, previousResults)), (rule, index)) => {
        evaluateRule(state1, env, conclusions6, rule) match {
          case (state2, iec @ RuleTyperEvaluateConflict(_, _, _)) => {
            return (state2, RuleTyperEvaluateConflict(conclusions6, "Failed evaluating rule index " + index, Some(iec)))
          }
          case (state2, RuleTyperEvaluateUnknown(conclusions7)) => {
            (state2, RuleTyperEvaluateUnknown(conclusions7))
          }
          case (state2, RuleTyperEvaluateSuccess(conclusions7, result)) => {
            (state2, RuleTyperEvaluateSuccess(conclusions7, previousResults :+ result))
          }
        }
      }
    })
  }

  def evaluateRuleCall(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    ruleCall: CallSR,
  ): (State, IRuleTyperEvaluateResult[CallAR]) = {
    val CallSR(name, argumentRules) = ruleCall

    name match {
//      case "ownership" => {
//        val List(CoordTemplata(coord)) = argTemplatas
//        (RuleTyperEvaluateSuccess(conclusions1, OwnershipTemplata(coord.ownership)))
//      }
//      case "mutability" => {
//        val List(KindTemplata(kind)) = argTemplatas
//        val mutability = delegate.getMutability(kind)
//        (RuleTyperEvaluateSuccess(conclusions1, MutabilityTemplata(mutability)))
//      }
      case "toRef" => {
        if (argumentRules.size != 1) {
          return (state0, RuleTyperEvaluateConflict(conclusions0, "toRef expects 1 argument, but received " + argumentRules.size, None))
        }
        val List(kindRule) = argumentRules
        makeMatcher().matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, kindRule) match {
          case (state1, rtmc @ RuleTyperMatchConflict(_, _, _)) => (state1, RuleTyperEvaluateConflict(conclusions0, "Conflict in toRef argument!", Some(rtmc)))
          case (state1, RuleTyperMatchSuccess(conclusions1, kindRuleT)) => {
            val ruleT = CallAR(name, List(kindRuleT), CoordTemplataType)
            (state1, RuleTyperEvaluateSuccess(conclusions1, ruleT))
          }
        }
      }
      case "passThroughIfInterface" => {
        if (argumentRules.size != 1) {
          return (state0, RuleTyperEvaluateConflict(conclusions0, "passThroughIfInterface expects 1 argument, but received " + argumentRules.size, None))
        }
        val List(kindRule) = argumentRules
        makeMatcher().matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, kindRule) match {
          case (state1, rtmc @ RuleTyperMatchConflict(_, _, _)) => (state1, RuleTyperEvaluateConflict(conclusions0, "Conflict in toRef argument!", Some(rtmc)))
          case (state1, RuleTyperMatchSuccess(conclusions1, kindRuleT)) => {
            val ruleT = CallAR(name, List(kindRuleT), KindTemplataType)
            (state1, RuleTyperEvaluateSuccess(conclusions1, ruleT))
          }
        }
      }
      case _ => vfail("Unknown function \"" + name + "\"!");
    }
  }

  def evaluateTemplex(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    ruleTemplex: ITemplexS,
  ): (State, IRuleTyperEvaluateResult[ITemplexA]) = {
    ruleTemplex match {
      case IntST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, IntAT(value)))
      case BoolST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, BoolAT(value)))
      case MutabilityST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, MutabilityAT(value)))
      case PermissionST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, PermissionAT(value)))
      case LocationST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, LocationAT(value)))
      case OwnershipST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, OwnershipAT(value)))
      case VariabilityST(value) => (state0, RuleTyperEvaluateSuccess(conclusions0, VariabilityAT(value)))
      case NameST(name) => {
        delegate.lookupType(state0, env, name) match {
          case (state1, KindTemplataType) => {
            // The thing identified by `name` is a kind, but we don't know whether we're trying to access it
            // as a kind, or trying to access it like a coord.
            // Kinds from the outside are ambiguous until we know from context whether we're trying to use
            // them like a kind or a coord.
            (state1, RuleTyperEvaluateUnknown(conclusions0))
          }
          case (state1, otherType) => {
            (state1, RuleTyperEvaluateSuccess(conclusions0, NameAT(name, otherType)))
          }
        }
      }
      case AnonymousRuneST() => (state0, RuleTyperEvaluateUnknown(conclusions0))
      case RuneST(rune) => {
        conclusions0.typeByRune.get(rune) match {
          case Some(tyype) => (state0, RuleTyperEvaluateSuccess(conclusions0, RuneAT(rune, tyype)))
          case None => (state0, RuleTyperEvaluateUnknown(conclusions0))
        }
      }
      case OwnershippedST(ownershipP, innerCoordTemplexS) => {
        makeMatcher().matchTypeAgainstTemplexS(state0, env, conclusions0, CoordTemplataType, innerCoordTemplexS) match {
          case (state15, rtmc @ RuleTyperMatchConflict(_, _, _)) => (state15, RuleTyperEvaluateConflict(conclusions0, "Conflict in inner coord part!", Some(rtmc)))
          case (state1, RuleTyperMatchUnknown(conclusions1)) => {
            (state1, RuleTyperEvaluateUnknown(conclusions1))
          }
          case (state15, RuleTyperMatchSuccess(conclusions1, kindTemplexT)) => {
            val templexT = OwnershippedAT(ownershipP, kindTemplexT)
            (state15, RuleTyperEvaluateSuccess(conclusions1, templexT))
          }
        }
      }
      case CallST(templateRule, paramRules) => {
        val (state2, conclusions2, maybeTemplateT) =
          evaluateTemplex(state0, env, conclusions0, templateRule) match {
            case (state1, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state1, RuleTyperEvaluateConflict(conclusions0, "bogglewogget", Some(iec)))
            case (state1, RuleTyperEvaluateUnknown(conclusions1)) => (state1, conclusions1, None)
            case (state1, RuleTyperEvaluateSuccess(conclusions1, templexT)) => {
              templexT.resultType match {
                case TemplateTemplataType(_, _) =>
                case _ => {
                  return (state1, RuleTyperEvaluateConflict(conclusions1, "Trying to call something that's not a template! Is actually: " + templexT.resultType, None))
                }
              }
              (state1, conclusions1, Some(templexT))
            }
          }

        maybeTemplateT match {
          case None => {
            // We don't know the template type, so we can't know the resulting type and can't assemble
            // the CallAR... but evaluating the arguments anyway might yield clues as to the types of
            // the runes, so evaluate them anyway.

            val (state20, conclusions20) =
              paramRules.zipWithIndex.foldLeft((state2, conclusions2))({
                case ((state11, conclusions11), (paramRule, paramIndex)) => {
                  evaluateTemplex(state11, env, conclusions11, paramRule) match {
                    case (state15, imc @ RuleTyperEvaluateConflict(_, _, _)) => return (state15, RuleTyperEvaluateConflict(conclusions11, "Conflict while evaluating param #" + paramIndex + "! " + paramRule, Some(imc)))
                    case (state15, RuleTyperEvaluateUnknown(conclusions5)) => (state15, conclusions5)
                    case (state15, RuleTyperEvaluateSuccess(conclusions5, paramRuleT)) => {
                      // Throw it away; without knowing the template type, even with the argument types
                      // we can't know the return type.
                      val (_) = paramRuleT

                      (state15, conclusions5)
                    }
                  }
                }
              })
            (state20, RuleTyperEvaluateUnknown(conclusions20))
          }
          case Some(templateT) => {
            val TemplateTemplataType(paramTypes, returnType) = templateT.resultType
            val (state20, conclusions20, maybeRulesT) =
              paramTypes.zip(paramRules).zipWithIndex.foldLeft((state2, conclusions2, List[Option[ITemplexA]]()))({
                case ((state11, conclusions11, previousMaybeRulesT), ((paramType, paramRule), paramIndex)) => {
                  makeMatcher().matchTypeAgainstTemplexS(state11, env, conclusions11, paramType, paramRule) match {
                    case (state15, imc @ RuleTyperMatchConflict(_, _, _)) => return (state15, RuleTyperEvaluateConflict(conclusions11, "Conflict while matching param #" + paramIndex + "! Was matching " + paramType + " and " + paramRule, Some(imc)))
                    case (state15, RuleTyperMatchUnknown(conclusions5)) => (state15, conclusions5, previousMaybeRulesT :+ None)
                    case (state15, RuleTyperMatchSuccess(conclusions5, paramTemplexT)) => (state15, conclusions5, previousMaybeRulesT :+ Some(paramTemplexT))
                  }
                }
              })

            if (maybeRulesT.contains(None)) {
              (state20, RuleTyperEvaluateUnknown(conclusions20))
            } else {
              returnType match {
                case KindTemplataType => {
                  // Return unknown, because we don't know if it should actually be a kind, or a coord.
                  // Only the matcher can figure this out.
                  (state20, RuleTyperEvaluateUnknown(conclusions20))
                }
                case _ => {
                  (state20, RuleTyperEvaluateSuccess(conclusions20, CallAT(templateT, maybeRulesT.flatten, returnType)))
                }
              }
            }
          }
        }
      }
      case PrototypeST(_, _, _) => {
        vfail("Unimplemented")
      }
      case PackST(_) => {
//        evaluateTemplexes(env, conclusions0, memberTemplexes) match {
//          case (iec @ RuleTyperEvaluateConflict(_, _, _)) => {
//            return (RuleTyperEvaluateConflict(conclusions0, "Failed to evaluate CallST arguments", Some(iec)))
//          }
//          case (RuleTyperEvaluateUnknown(conclusions3)) => {
//            (RuleTyperEvaluateUnknown(conclusions3))
//          }
//          case (RuleTyperEvaluateSuccess(conclusions3, memberTemplatas)) => {
//            val memberCoords = memberTemplatas.collect({ case CoordTemplata(coord) => coord })
//            if (memberCoords.size != memberTemplatas.size) {
//              vfail("Packs can only take coords!")
//            }
//
//            val (packKind, _) = delegate.getPackKind(env, memberCoords)
//            (RuleTyperEvaluateSuccess(conclusions3, KindTemplata(packKind)))
//          }
//        }
        vfail()
      }
      case RepeaterSequenceST(mutabilityTemplexS, sizeTemplexS, elementTemplexS) => {
        // It's futile to try and get the templexTs for size and element, since we don't know whether this
        // thing will end up as a kind or coord (only matching can know that) but hey, let's match into
        // them anyway, they might provide some nice intel for our conclusions.

        val (state10, conclusions10) =
          makeMatcher().matchTypeAgainstTemplexS(state0, env, conclusions0, MutabilityTemplataType, mutabilityTemplexS) match {
            case (state3, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state3, RuleTyperEvaluateConflict(conclusions0, "Conflict in mutability part!", Some(rtmc)))
            case (state3, RuleTyperMatchUnknown(conclusions3)) => (state3, conclusions3)
            case (state3, RuleTyperMatchSuccess(conclusions3, _)) => (state3, conclusions3)
          }
        val (state20, conclusions20) =
          makeMatcher().matchTypeAgainstTemplexS(state10, env, conclusions10, IntegerTemplataType, sizeTemplexS) match {
            case (state13, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Conflict in element part!", Some(rtmc)))
            case (state13, RuleTyperMatchUnknown(conclusions13)) => (state13, conclusions13)
            case (state13, RuleTyperMatchSuccess(conclusions13, _)) => (state13, conclusions13)
          }
        val (state30, conclusions30) =
          makeMatcher().matchTypeAgainstTemplexS(state20, env, conclusions20, CoordTemplataType, elementTemplexS) match {
            case (state23, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state23, RuleTyperEvaluateConflict(conclusions20, "Conflict in element part!", Some(rtmc)))
            case (state23, RuleTyperMatchUnknown(conclusions23)) => (state23, conclusions23)
            case (state23, RuleTyperMatchSuccess(conclusions23, _)) => (state23, conclusions23)
          }

        // We don't know whether this thing is expected to be a kind or a coord, only matching can figure that out.
        // Return unknown.
        (state30, RuleTyperEvaluateUnknown(conclusions30))
      }
      case ManualSequenceST(_) => {
        vfail("Unimplemented")
      }
    }
  }

  def evaluateTypedRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: TypedSR,
  ): (State, IRuleTyperEvaluateResult[TemplexAR]) = {
    val TypedSR(maybeRune, typeSR) = rule

    val templataType =
      typeSR match {
        case CoordTypeSR => CoordTemplataType
        case KindTypeSR => KindTemplataType
        case MutabilityTypeSR => MutabilityTemplataType
      }

    maybeRune match {
      case None =>
      case Some(rune) => {
        conclusions0.typeByRune.get(rune) match {
          case None =>
          case Some(typeFromConclusions) => {
            if (typeFromConclusions != templataType) {
              return (state0, RuleTyperEvaluateConflict(conclusions0, "Typed rule failed: expected rune " + rune + " to be " + templataType + " but previously concluded " + typeFromConclusions, None))
            }
          }
        }
      }
    }

    makeMatcher().matchTypeAgainstTypedSR(state0, env, conclusions0, templataType, rule) match {
      case (state5, imc @ RuleTyperMatchConflict(_, _, _)) => (state5, RuleTyperEvaluateConflict(conclusions0, "", Some(imc)))
      case (state5, RuleTyperMatchUnknown(conclusions5)) => (state5, RuleTyperEvaluateUnknown(conclusions5))
      case (state5, RuleTyperMatchSuccess(conclusions5, ruleT)) => (state5, RuleTyperEvaluateSuccess(conclusions5, ruleT))
    }
  }

  def evaluateIsaRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: IsaSR,
  ): (State, IRuleTyperEvaluateResult[IsaAR]) = {
    val IsaSR(leftRuleS, rightRuleS) = rule

    val (state30, conclusions30, maybeLeftRuleT) =
      makeMatcher().matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, leftRuleS) match {
        case (state23, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state23, RuleTyperEvaluateConflict(conclusions0, "Failed matching isa's left rule", Some(rtmc)))
        case (state23, RuleTyperMatchUnknown(conclusions23)) => (state23, conclusions23, None)
        case (state23, RuleTyperMatchSuccess(conclusions23, leftRuleT)) => (state23, conclusions23, Some(leftRuleT))
      }

    val (state40, conclusions40, maybeRightRuleT) =
      makeMatcher().matchTypeAgainstRulexSR(state30, env, conclusions30, KindTemplataType, rightRuleS) match {
        case (state33, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state33, RuleTyperEvaluateConflict(conclusions30, "Failed matching isa's right rule", Some(rtmc)))
        case (state33, RuleTyperMatchUnknown(conclusions33)) => (state33, conclusions33, None)
        case (state33, RuleTyperMatchSuccess(conclusions33, rightRuleT)) => (state33, conclusions33, Some(rightRuleT))
      }

    (maybeLeftRuleT, maybeRightRuleT) match {
      case (Some(leftRuleT), Some(rightRuleT)) => (state40, RuleTyperEvaluateSuccess(conclusions40, IsaAR(leftRuleT, rightRuleT)))
      case (_, _) => (state40, RuleTyperEvaluateUnknown(conclusions40))
    }
  }

  def evaluateEqualsRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: EqualsSR,
  ): (State, IRuleTyperEvaluateResult[EqualsAR]) = {
    val EqualsSR(leftRuleS, rightRuleS) = rule

    val (state10, conclusions10, maybeLeftRuleT) =
      evaluateRule(state0, env, conclusions0, leftRuleS) match {
        case (state1, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state1, RuleTyperEvaluateConflict(conclusions0, "Failed evaluating left rule!", Some(iec)))
        case (state1, RuleTyperEvaluateUnknown(conclusions3)) => (state1, conclusions3, None)
        case (state1, RuleTyperEvaluateSuccess(conclusions3, leftRuleT)) => (state1, conclusions3, Some(leftRuleT))
      }

    val (state20, conclusions20, maybeRightRuleT) =
      evaluateRule(state10, env, conclusions10, rightRuleS) match {
        case (state13, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed evaluating right rule!", Some(iec)))
        case (state13, RuleTyperEvaluateUnknown(conclusions13)) => (state13, conclusions13, None)
        case (state13, RuleTyperEvaluateSuccess(conclusions13, rightRuleT)) => (state13, conclusions13, Some(rightRuleT))
      }

    (maybeLeftRuleT, maybeRightRuleT) match {
      case (Some(leftRuleT), Some(rightRuleT)) => {
        (state20, RuleTyperEvaluateSuccess(conclusions20, EqualsAR(leftRuleT, rightRuleT)))
      }
      case (Some(leftRuleT), None) => {
        // We know the left, but don't know the right. Use the type from the left
        // to try and figure out the thing on the right.
        makeMatcher().matchTypeAgainstRulexSR(state20, env, conclusions20, leftRuleT.resultType, rightRuleS) match {
          case (state23, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state23, RuleTyperEvaluateConflict(conclusions20, "Failed matching right rule with type from left (" + leftRuleT.resultType + ")", Some(rtmc)))
          case (state23, RuleTyperMatchUnknown(conclusions23)) => (state23, RuleTyperEvaluateUnknown(conclusions23))
          case (state23, RuleTyperMatchSuccess(conclusions23, rightRuleT)) => {
            (state23, RuleTyperEvaluateSuccess(conclusions23, EqualsAR(leftRuleT, rightRuleT)))
          }
        }
      }
      case (None, Some(rightRuleT)) => {
        // We know the left, but don't know the right. Use the type from the left
        // to try and figure out the thing on the right.
        makeMatcher().matchTypeAgainstRulexSR(state20, env, conclusions20, rightRuleT.resultType, leftRuleS) match {
          case (state23, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state23, RuleTyperEvaluateConflict(conclusions20, "Failed matching left rule with type from right (" + rightRuleT.resultType + ")", Some(rtmc)))
          case (state23, RuleTyperMatchUnknown(conclusions23)) => (state23, RuleTyperEvaluateUnknown(conclusions23))
          case (state23, RuleTyperMatchSuccess(conclusions23, leftRuleT)) => {
            (state23, RuleTyperEvaluateSuccess(conclusions23, EqualsAR(leftRuleT, rightRuleT)))
          }
        }
      }
      case (None, None) => {
        (state20, RuleTyperEvaluateUnknown(conclusions20))
      }
    }
  }

  def evaluateOrRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: OrSR
  ): (State, IRuleTyperEvaluateResult[OrAR]) = {
    val (state50, conclusions50, listOfMaybeAlternativeT) =
      rule.alternatives.zipWithIndex.foldLeft((state0, conclusions0, List[Option[IRulexAR]]()))({
        case ((state10, conclusions10, Nil), (alternative, alternativeIndex)) => {
          evaluateRule(state10, env, conclusions10, alternative) match {
            case (state13, rtec @ RuleTyperEvaluateConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed to evaluate alternative index " + alternativeIndex, Some(rtec)))
            case (state13, RuleTyperEvaluateSuccess(conclusions13, alternativeRuleT)) => {
              (state13, conclusions13, List(Some(alternativeRuleT)))
            }
            case (state13, RuleTyperEvaluateUnknown(conclusions13)) => {
              (state13, conclusions13, List(None))
            }
          }
        }
        case ((state10, conclusions10, previousMaybeAlternativesT), (alternative, alternativeIndex)) => {
          val maybeKnownType = previousMaybeAlternativesT.flatten.headOption.map(_.resultType)
          maybeKnownType match {
            case None => {
              evaluateRule(state10, env, conclusions10, alternative) match {
                case (state13, rtec @ RuleTyperEvaluateConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed to evaluate alternative index " + alternativeIndex, Some(rtec)))
                case (state13, RuleTyperEvaluateUnknown(conclusions13)) => {
                  (state13, conclusions13, previousMaybeAlternativesT :+ None)
                }
                case (state13, RuleTyperEvaluateSuccess(conclusions13, alternativeRuleT)) => {
                  (state13, conclusions13, previousMaybeAlternativesT :+ Some(alternativeRuleT))
                }
              }
            }
            case Some(knownType) => {
              makeMatcher().matchTypeAgainstRulexSR(state10, env, conclusions10, knownType, alternative) match {
                case (state13, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed to evaluate alternative index " + alternativeIndex, Some(rtmc)))
                case (state13, RuleTyperMatchUnknown(conclusions13)) => {
                  (state13, conclusions13, previousMaybeAlternativesT :+ None)
                }
                case (state13, RuleTyperMatchSuccess(conclusions13, alternativeRuleT)) => {
                  (state13, conclusions13, previousMaybeAlternativesT :+ Some(alternativeRuleT))
                }
              }
            }
          }
        }
      })
    if (listOfMaybeAlternativeT.contains(None)) {
      (state50, RuleTyperEvaluateUnknown(conclusions50))
    } else {
      val alternativesT = listOfMaybeAlternativeT.flatten
      (state50, RuleTyperEvaluateSuccess(conclusions50, OrAR(alternativesT)))
    }
  }

  def evaluateComponentsRule(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    rule: ComponentsSR,
  ): (State, IRuleTyperEvaluateResult[EqualsAR]) = {
    val ComponentsSR(typedRule, components) = rule

    val (state10, conclusions10, maybeTypeAndRuneRuleT) =
      evaluateTypedRule(state0, env, conclusions0, typedRule) match {
        case (state3, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state3, RuleTyperEvaluateConflict(conclusions0, "Components rule type disagrees!", Some(iec)))
        case (state3, RuleTyperEvaluateUnknown(conclusions3)) => (state3, conclusions3, None)
        case (state3, RuleTyperEvaluateSuccess(conclusions3, typeAndRuneRuleT)) => (state3, conclusions3, Some(typeAndRuneRuleT))
      }

    val (state50, conclusions50, maybeComponentRulesT) =
      typedRule.tyype match {
        case KindTypeSR => {
          evaluateKindComponents(state10, env, conclusions10, components) match {
            case (state13, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed evaluating kind components!", Some(iec)))
            case (state13, RuleTyperEvaluateUnknown(conclusions13)) => (state13, conclusions13, None)
            case (state13, RuleTyperEvaluateSuccess(conclusions13, templataFromRune)) => (state13, conclusions13, Some(templataFromRune))
          }
        }
        case CoordTypeSR => {
          evaluateCoordComponents(state10, env, conclusions10, components) match {
            case (state13, iec @ RuleTyperEvaluateConflict(_, _, _)) => return (state13, RuleTyperEvaluateConflict(conclusions10, "Failed evaluating coord components!", Some(iec)))
            case (state13, RuleTyperEvaluateUnknown(conclusions13)) => (state13, conclusions13, None)
            case (state13, RuleTyperEvaluateSuccess(conclusions13, templataFromRune)) => (state13, conclusions13, Some(templataFromRune))
          }
        }
        case _ => vfail("Can only destructure coords and kinds!")
      }

    (maybeTypeAndRuneRuleT, maybeComponentRulesT) match {
      case (Some(typeAndRuneRuleT), Some(componentRulesT)) => {
        val equalsT =
          EqualsAR(
            typeAndRuneRuleT,
            ComponentsAR(typeAndRuneRuleT.resultType, componentRulesT))
        (state50, RuleTyperEvaluateSuccess(conclusions50, equalsT))
      }
      case (None, None) => {
        (state50, RuleTyperEvaluateUnknown(conclusions50))
      }
    }
  }

  private def evaluateCoordComponents(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    components: List[IRulexSR]):
  (State, IRuleTyperEvaluateResult[List[IRulexAR]]) = {
    components match {
      case List(ownershipRuleS, kindRuleS) => {
        val (state10, conclusions10, maybeOwnershipRuleT) =
          makeMatcher().matchTypeAgainstRulexSR(state0, env, conclusions0, OwnershipTemplataType, ownershipRuleS) match {
            case (state1, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state1, RuleTyperEvaluateConflict(conclusions0, "Ownership component conflicted!", Some(rtmc)))
            case (state1, RuleTyperMatchUnknown(c)) => (state1, c, None)
            case (state1, RuleTyperMatchSuccess(c, ownershipRuleT)) => (state1, c, Some(ownershipRuleT))
          }
//        val (conclusions20, maybeLocationRuleT) =
//          makeMatcher().matchTypeAgainstRulexSR(env, conclusions10, LocationTemplataType, locationRuleS) match {
//            case rtmc @ RuleTyperMatchConflict(_, _, _) => return RuleTyperEvaluateConflict(conclusions10, "Location component conflicted!", Some(rtmc))
//            case RuleTyperMatchUnknown(c) => (c, None)
//            case RuleTyperMatchContinue(c, locationRuleT) => (c, Some(locationRuleT))
//          }
//        val (conclusions30, maybePermissionRuleT) =
//          makeMatcher().matchTypeAgainstRulexSR(env, conclusions20, PermissionTemplataType, permissionRuleS) match {
//            case rtmc @ RuleTyperMatchConflict(_, _, _) => return RuleTyperEvaluateConflict(conclusions20, "Permission component conflicted!", Some(rtmc))
//            case RuleTyperMatchUnknown(c) => (c, None)
//            case RuleTyperMatchContinue(c, permissionRuleT) => (c, Some(permissionRuleT))
//          }
        val (state40, conclusions40, maybeKindRuleT) =
          makeMatcher().matchTypeAgainstRulexSR(state10, env, conclusions10, KindTemplataType, kindRuleS) match {
            case (state11, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state11, RuleTyperEvaluateConflict(conclusions10, "Kind component conflicted!", Some(rtmc)))
            case (state11, RuleTyperMatchUnknown(c)) => (state11, c, None)
            case (state11, RuleTyperMatchSuccess(c, kindRuleT)) => (state11, c, Some(kindRuleT))
          }
        (maybeOwnershipRuleT, maybeKindRuleT) match {
          case (Some(ownershipRuleT), Some(kindRuleT)) => {
            (state40, RuleTyperEvaluateSuccess(conclusions40, List(ownershipRuleT, kindRuleT)))
          }
          case (_, _) => {
            (state40, RuleTyperEvaluateUnknown(conclusions40))
          }
        }

      }
      case _ => vfail("Coords must have 2 components")
    }
  }

  private def evaluateKindComponents(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    components: List[IRulexSR]):
  (State, IRuleTyperEvaluateResult[List[IRulexAR]]) = {
    components match {
      case List(mutabilityRule) => {
        val (state10, conclusions10, maybeMutabilityRule) =
          makeMatcher().matchTypeAgainstRulexSR(state0, env, conclusions0, MutabilityTemplataType, mutabilityRule) match {
            case (state3, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state3, RuleTyperEvaluateConflict(conclusions0, "Mutability component conflicted!", Some(rtmc)))
            case (state3, RuleTyperMatchUnknown(c)) => (state3, c, None)
            case (state3, RuleTyperMatchSuccess(c, mutabilityRuleT)) => (state3, c, Some(mutabilityRuleT))
          }
        maybeMutabilityRule match {
          case None => (state10, RuleTyperEvaluateUnknown(conclusions10))
          case Some(mutabilityRuleT) => (state10, RuleTyperEvaluateSuccess(conclusions10, List(mutabilityRuleT)))
        }
      }
      case _ => vfail("Kind rule must have one component")
    }
  }
  
  private def makeMatcher() = {
    new RuleTyperMatcher[Env, State](
      evaluateTemplex,
      new RuleTyperMatcherDelegate[Env, State] {
        override def lookupType(state: State, env: Env, name: String): (State, ITemplataType) = {
          delegate.lookupType(state, env, name)
        }
      })
  }

}
