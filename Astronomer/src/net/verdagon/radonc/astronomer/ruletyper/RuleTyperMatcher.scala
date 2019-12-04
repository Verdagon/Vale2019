package net.verdagon.radonc.astronomer.ruletyper

import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc._
import net.verdagon.radonc.astronomer._

import scala.collection.immutable.List

trait RuleTyperMatcherDelegate[Env, State] {
  def lookupType(state0: State, env: Env, name: String): (State, ITemplataType)
}

class RuleTyperMatcher[Env, State](
    evaluate: (State, Env, Conclusions, ITemplexS) => (State, IRuleTyperEvaluateResult[ITemplexA]),
    delegate: RuleTyperMatcherDelegate[Env, State]) {
  private def addConclusion(
    conclusions0: Conclusions,
    rune: String,
    tyype: ITemplataType):
  IRuleTyperMatchResult[Unit] = {
    conclusions0.typeByRune.get(rune) match {
      case None => RuleTyperMatchSuccess(conclusions0.addConclusion(rune, tyype), ())
      case Some(existing) => {
        if (existing == tyype) {
          RuleTyperMatchSuccess(conclusions0, ())
        } else {
          RuleTyperMatchConflict(conclusions0, "Disagreement about rune " + rune + "! " + existing + " and " + tyype, List())
        }
      }
    }
  }

  def matchAgainstDestructure(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    parts: List[Option[AtomSP]]):
  (State, IRuleTyperMatchResult[Unit]) = {
    val (state10, conclusions10) =
      parts.foldLeft((state0, conclusions0))({
        case ((state2, conclusions2), None) => {
          // The part is None; there's just an _ in this spot of the destructure.
          // Nothing to do here.
          (state2, conclusions2)
        }
        case ((state2, conclusions2), Some(part)) => {
          matchAgainstAtomSP(state2, env, conclusions2, part) match {
            case (state3, imc @ RuleTyperMatchConflict(_, _, _)) => return (state3, imc)
            case (state3, RuleTyperMatchSuccess(conclusions4, ())) => (state3, conclusions4)
          }
        }
      })
    (state10, RuleTyperMatchSuccess(conclusions10, ()))
  }

  def matchAgainstAtomSP(
      state0: State,
      env: Env,
      conclusions0: Conclusions,
      rule: AtomSP):
  (State, IRuleTyperMatchResult[Unit]) = {
    val conclusions2 =
      addConclusion(conclusions0, rule.coordRune, CoordTemplataType) match {
        case (imc @ RuleTyperMatchConflict(_, _, _)) => return (state0, imc)
        case (RuleTyperMatchSuccess(conclusions1, ())) => conclusions1
      }

    val (state4, conclusions4) =
      rule.destructure match {
        case None => (state0, conclusions2)
        case Some(parts) => {
          matchAgainstDestructure(state0, env, conclusions0, parts) match {
            case (state3, imc @ RuleTyperMatchConflict(_, _, _)) => return (state3, imc)
            case (state3, RuleTyperMatchSuccess(conclusions3, ())) => (state3, conclusions3)
          }
        }
      }

    val conclusions10 =
      rule.virtuality match {
        case None => conclusions4
        case Some(AbstractSP) => conclusions4
        case Some(OverrideSP(kindRune)) => {
          addConclusion(conclusions4, kindRune, KindTemplataType) match {
            case (imc @ RuleTyperMatchConflict(_, _, _)) => return (state4, imc)
            case (RuleTyperMatchSuccess(conclusions5, ())) => conclusions5
          }
        }
      }

    (state4, RuleTyperMatchSuccess(conclusions10, ()))
  }

//  def matchCitizenAgainstCallST(
//    env: Env,
//    conclusions0: Conclusions,
//    expectedTemplate: ITemplexS,
//    expectedArgs: List[ITemplexS],
//    actualTemplateName: String,
//    actualArgs: List[ITemplata]):
//  IRuleTyperMatchResult = {
//    val actualTemplate =
//      delegate.lookupTemplata(env, actualTemplateName) match {
//        case None => return (RuleTyperMatchConflict(conclusions0, s"Couldn't find template '${actualTemplateName}'", List()))
//        case Some(x) => x
//      }
//    // Check to see that the actual template matches the expected template
//    val conclusions2 =
//      matchTypeAgainstTemplexSR(env, conclusions0, actualTemplate, expectedTemplate) match {
//        case (imc @ RuleTyperMatchConflict(_, _, _)) => return imc
//        case (RuleTyperMatchContinue(conclusions1)) => conclusions1
//      }
//    // Check to see that the actual template args match the expected template args
//    val conclusions5 =
//      expectedArgs.zip(actualArgs).foldLeft(conclusions2)({
//        case (conclusions3, (expectedArg, actualArg)) => {
//          matchTypeAgainstTemplexSR(env, conclusions3, actualArg, expectedArg) match {
//            case (imc @ RuleTyperMatchConflict(_, _, _)) => return imc
//            case (RuleTyperMatchContinue(conclusions4)) => conclusions4
//          }
//        }
//      })
//    // If the function is the same, and the args are the same... it's the same.
//    (RuleTyperMatchContinue(conclusions5))
//  }

  def matchTypeAgainstTemplexesS(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedTypes: List[ITemplataType],
    rules: List[ITemplexS]):
  (State, IRuleTyperMatchResult[List[ITemplexA]]) = {
    vassert(expectedTypes.size == rules.size)
    val (state20, conclusions20, resultTemplexesT) =
      expectedTypes.zip(rules).zipWithIndex.foldLeft((state0, conclusions0, List[ITemplexA]()))({
        case ((state1, conclusions1, previousResults), ((expectedType, rule), index)) => {
          matchTypeAgainstTemplexS(state1, env, conclusions1, expectedType, rule) match {
            case (state2, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state2, RuleTyperMatchConflict(conclusions1, "Failed evaluating templex " + index, List(rtmc)))
            case (state2, RuleTyperMatchUnknown(conclusions2)) => (state2, conclusions2, previousResults)
            case (state2, RuleTyperMatchSuccess(conclusions2, templexT)) => (state2, conclusions2, previousResults :+ templexT)
          }
        }
      })
    if (resultTemplexesT.size == rules.size) {
      (state20, RuleTyperMatchSuccess(conclusions20, resultTemplexesT))
    } else {
      (state20, RuleTyperMatchUnknown(conclusions20))
    }
  }

  def matchTypeAgainstTemplexS(
      state0: State,
      env: Env,
      conclusions0: Conclusions,
      expectedType: ITemplataType,
      rule: ITemplexS):
  (State, IRuleTyperMatchResult[ITemplexA]) = {
    (rule, expectedType) match {
      case (AnonymousRuneST(), _) => (state0, RuleTyperMatchSuccess(conclusions0, AnonymousRuneAT(expectedType)))
      case (IntST(value), IntegerTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, IntAT(value)))
      case (BoolST(value), BooleanTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, BoolAT(value)))
      case (MutabilityST(value), MutabilityTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, MutabilityAT(value)))
      case (PermissionST(value), PermissionTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, PermissionAT(value)))
      case (LocationST(value), LocationTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, LocationAT(value)))
      case (OwnershipST(value), OwnershipTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, OwnershipAT(value)))
      case (VariabilityST(value), VariabilityTemplataType) => (state0, RuleTyperMatchSuccess(conclusions0, VariabilityAT(value)))
      case (NameST(name), _) => {
        val (state1, tyype) = delegate.lookupType(state0, env, name)
        // Add something to this case to note that we've added it, and all its combinations,
        // to the main match below.
        tyype match {
//          case TemplateTemplataType(_, _) =>
          case KindTemplataType =>
          case MutabilityTemplataType =>
          case CoordTemplataType =>
          case TemplateTemplataType(_, _) => // We check for strict equality, nothing fancy here.
          case _ => vfail()
        }
        // Add something to this case to note that we've added it, and all its combinations,
        // to the main match below.
        expectedType match {
//          case TemplateTemplataType(_, _) =>
          case KindTemplataType =>
          case CoordTemplataType =>
          case MutabilityTemplataType =>
          case IntegerTemplataType =>
          case TemplateTemplataType(_, _) => // We check for strict equality, nothing fancy here.
          case _ => vfail(expectedType.toString)
        }
        // When something's missing, consider all of the combinations it has with everything
        // else, then once youve considered them, add them to the above matches.
        (tyype, expectedType) match {
          case (IntegerTemplataType, IntegerTemplataType) => {
            (state1, RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType)))
          }
          case (nonIntType, IntegerTemplataType) => {
            (state1, RuleTyperMatchConflict(conclusions0, "Expected an int, but was " + nonIntType, List()))
          }
          case (MutabilityTemplataType, MutabilityTemplataType) => {
            (state1, RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType)))
          }
          case (CoordTemplataType, CoordTemplataType) => {
            (state1, RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType)))
          }
          case (KindTemplataType, KindTemplataType | CoordTemplataType) => {
            (state1, RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType)))
          }
          case (TemplateTemplataType(paramTypes, returnType), TemplateTemplataType(expectedParamTypes, expectedReturnType)) => {
            if (paramTypes.size != expectedParamTypes.size) {
              return (state1, RuleTyperMatchConflict(conclusions0, "Received " + paramTypes.size + " template params but expected " + expectedParamTypes.size, List()))
            }
            if (paramTypes != expectedParamTypes) {
              return (state1, RuleTyperMatchConflict(conclusions0, "Received " + paramTypes + " template params but expected " + expectedParamTypes, List()))
            }
            if (returnType != expectedReturnType) {
              return (state1, RuleTyperMatchConflict(conclusions0, "Received " + returnType + " return type but expected " + expectedReturnType, List()))
            }
            (state1, RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType)))
          }
//          // Is this right? Can't we look it up as a coord, like we did with KindTemplata/CoordTemplataType?
//          case (InterfaceTemplata(_, interfaceS), KindTemplataType | CoordTemplataType) => {
//            if (Inferer.interfaceIsTemplate(interfaceS)) {
//              RuleTyperMatchConflict(conclusions0, "Tried making a '" + name + "' but it's a template and no arguments were supplied!", List())
//            } else {
//              RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType))
//            }
//          }
//          // Is this right? Can't we look it up as a coord, like we did with KindTemplata/CoordTemplataType?
//          case (StructTemplata(_, structS), KindTemplataType | CoordTemplataType) => {
//            if (Inferer.structIsTemplate(structS)) {
//              RuleTyperMatchConflict(conclusions0, "Tried making a '" + name + "' but it's a template and no arguments were supplied!", List())
//            } else {
//              RuleTyperMatchSuccess(conclusions0, NameAT(name, expectedType))
//            }
//          }
//          case (it @ InterfaceTemplata(_, _), TemplateTemplataType(paramTypes, KindTemplataType)) => {
//            val TemplateTemplataType(paramTypes, resultType) = delegate.getInterfaceTemplataType(it)
//            vimpl()
//          }
//          case (st @ StructTemplata(_, _), TemplateTemplataType(paramTypes, KindTemplataType)) => {
//            val TemplateTemplataType(paramTypes, resultType) = delegate.getStructTemplataType(st)
//            vimpl()
//          }
          case _ => (state1, RuleTyperMatchConflict(conclusions0, "'" + name + "' doesn't match needed " + expectedType, List()))
        }
      }
      case (RuneST(rune), _) => {
        addConclusion(conclusions0, rune, expectedType) match {
          case (imc @ RuleTyperMatchConflict(_, _, _)) => return (state0, RuleTyperMatchConflict(conclusions0, "Conflict in rune!", List(imc)))
          case (RuleTyperMatchSuccess(conclusions1, ())) => {
            (state0, RuleTyperMatchSuccess(conclusions1, RuneAT(rune, expectedType)))
          }
        }
      }
//      case (CallST(_, _), CoordTemplata(Coord(_, _))) => {
//        // is this where we do coercion to get to something like the below case?
//        vfail("impl?")
//      }
      case (CallST(template, templateArgs), KindTemplataType | CoordTemplataType) => {
        val (state10, conclusions10, maybeTemplateTemplexT) =
          evaluate(state0, env, conclusions0, template) match {
            case (state1, rtec @ RuleTyperEvaluateConflict(_, _, _)) => return (state1, RuleTyperMatchConflict(conclusions0, "Couldn't evaluate callee template!", List(rtec)))
            case (state1, RuleTyperEvaluateUnknown(conclusions1)) => (state1, conclusions1, None)
            case (state1, RuleTyperEvaluateSuccess(conclusions1, templateT)) => (state1, conclusions1, Some(templateT))
          }

        maybeTemplateTemplexT match {
          case None => {
            // We couldn't figure out the template, so we don't even know what the args are supposed to be.
            // But try evaluating them anyway, maybe that'll provide some nice clues to the types of some
            // runes.
            val (state20, conclusions20) =
              templateArgs.zipWithIndex.foldLeft((state10, conclusions10))({
                case ((state11, conclusions11), (templateArg, index)) => {
                  evaluate(state11, env, conclusions11, templateArg) match {
                    case (state12, rtec @ RuleTyperEvaluateConflict(_, _, _)) => return (state12, RuleTyperMatchConflict(conclusions0, "Couldn't evaluate template call arg " + index, List(rtec)))
                    case (state12, RuleTyperEvaluateUnknown(conclusions12)) => (state12, conclusions12)
                    case (state12, RuleTyperEvaluateSuccess(conclusions12, templateArgT)) => {
                      // We can't do anything with the templateArgT anyway because we don't have the
                      // template to bundle it with into a call, throw it away.
                      val (_) = templateArgT

                      (state12, conclusions12)
                    }
                  }
                }
              })
            (state20, RuleTyperMatchUnknown(conclusions20))
          }
          case Some(templateTemplexT) => {
            val templexTemplateType =
              templateTemplexT.resultType match {
                case ttt @ TemplateTemplataType(_, _) => ttt
                case _ => return (state10, RuleTyperMatchConflict(conclusions0, "Expected template call callee's to be a template but was " + templateTemplexT.resultType, List()))
              }

            (templexTemplateType.returnType, expectedType) match {
              case (a, b) if a == b =>
              // We can coerce kinds to coords, that's fine
              case (KindTemplataType, CoordTemplataType) =>
              case _ => return (state10, RuleTyperMatchConflict(conclusions0, "Expected template call callee's return type to be " + expectedType + " but was " + templexTemplateType.returnType, List()))
            }
            matchTypeAgainstTemplexesS(state10, env, conclusions10, templexTemplateType.paramTypes, templateArgs) match {
              case (state11, rtec @ RuleTyperMatchConflict(_, _, _)) => return (state11, RuleTyperMatchConflict(conclusions0, "Couldn't evaluate template call args!", List(rtec)))
              case (state11, RuleTyperMatchUnknown(conclusions11)) => {
                (state11, RuleTyperMatchUnknown(conclusions11))
              }
              case (state11, RuleTyperMatchSuccess(conclusions11, argTemplexesT)) => {
                (state11, RuleTyperMatchSuccess(conclusions11, CallAT(templateTemplexT, argTemplexesT, expectedType)))
              }
            }
          }
        }
      }
      case (PackST(List()), KindTemplataType | CoordTemplataType) => {
        (state0, RuleTyperMatchSuccess(conclusions0, PackAT(List(), expectedType)))
      }
//      case (CallST(expectedTemplate, expectedArgs), KindTemplata(InterfaceRef2(actualTemplateName, actualArgs))) => {
//        matchCitizenAgainstCallST(env, conclusions0, expectedTemplate, expectedArgs, actualTemplateName, actualArgs)
//      }
//      case (PrototypeST(_, _, _), _) => {
//        vfail("what even is this")
//      }
//      case (PackST(expectedMembers), KindTemplata(PackT2(actualMembers, _))) => {
//        val conclusions5 =
//          expectedMembers.zip(actualMembers).foldLeft(conclusions0)({
//            case (conclusions1, (expectedMember, actualMember)) => {
//              matchTypeAgainstTemplexSR(env, conclusions1, CoordTemplata(actualMember), expectedMember) match {
//                case (imc @ RuleTyperMatchConflict(_, _, _)) => return imc
//                case (RuleTyperMatchContinue(conclusions2)) => conclusions2
//              }
//            }
//          })
//        (RuleTyperMatchContinue(conclusions5))
//      }
//      case (RepeaterSequenceST(_, _), _) => {
//        vfail("impl")
//      }
//      case (OwnershipST(ownershipP), OwnershipTemplata(ownershipT)) => {
//        if (ownershipT != Conversions.evaluateOwnership(ownershipP)) {
//          (RuleTyperMatchConflict(conclusions0, s"Ownerships don't match: ${ownershipP} and ${ownershipT}", List()))
//        } else {
//          (RuleTyperMatchContinue(conclusions0))
//        }
//      }
      case (OwnershippedST(ownership, coordTemplex), CoordTemplataType) => {
        matchTypeAgainstTemplexS(state0, env, conclusions0, CoordTemplataType, coordTemplex) match {
          case (state11, rtec @ RuleTyperMatchConflict(_, _, _)) => return (state11, RuleTyperMatchConflict(conclusions0, "Couldn't evaluate ownershipped's kind!", List(rtec)))
          case (state11, RuleTyperMatchUnknown(conclusions11)) => (state11, RuleTyperMatchUnknown(conclusions11))
          case (state11, RuleTyperMatchSuccess(conclusions11, innerCoordRuleT)) => {
            (state11, RuleTyperMatchSuccess(conclusions11, OwnershippedAT(ownership, innerCoordRuleT)))
          }
        }
      }
      case (RepeaterSequenceST(mutabilityTemplexS, sizeTemplexS, elementTemplexS), KindTemplataType | CoordTemplataType) => {
        val (state10, conclusions10, maybeMutabilityTemplexT) =
          matchTypeAgainstTemplexS(state0, env, conclusions0, MutabilityTemplataType, mutabilityTemplexS) match {
            case (state3, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state3, RuleTyperMatchConflict(conclusions0, "Conflict in mutability part!", List(rtmc)))
            case (state3, RuleTyperMatchUnknown(conclusions3)) => (state3, conclusions3, None)
            case (state3, RuleTyperMatchSuccess(conclusions3, sizeTemplexT)) => (state3, conclusions3, Some(sizeTemplexT))
          }
        val (state20, conclusions20, maybeSizeTemplexT) =
          matchTypeAgainstTemplexS(state10, env, conclusions10, IntegerTemplataType, sizeTemplexS) match {
            case (state13, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state13, RuleTyperMatchConflict(conclusions0, "Conflict in size part!", List(rtmc)))
            case (state13, RuleTyperMatchUnknown(conclusions13)) => (state13, conclusions13, None)
            case (state13, RuleTyperMatchSuccess(conclusions13, sizeTemplexT)) => (state13, conclusions13, Some(sizeTemplexT))
          }
        val (state30, conclusions30, maybeElementTemplexT) =
          matchTypeAgainstTemplexS(state20, env, conclusions20, CoordTemplataType, elementTemplexS) match {
            case (state23, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state23, RuleTyperMatchConflict(conclusions20, "Conflict in element part!", List(rtmc)))
            case (state23, RuleTyperMatchUnknown(conclusions23)) => (state23, conclusions23, None)
            case (state23, RuleTyperMatchSuccess(conclusions23, elementTemplexT)) => (state23, conclusions23, Some(elementTemplexT))
          }
        (maybeMutabilityTemplexT, maybeSizeTemplexT, maybeElementTemplexT) match {
          case (Some(mutabilityTemplexT), Some(sizeTemplexT), (Some(elementTemplexT))) => {
            (state30, RuleTyperMatchSuccess(conclusions30, RepeaterSequenceAT(mutabilityTemplexT, sizeTemplexT, elementTemplexT, expectedType)))
          }
          case (_, _, _) => (state20, RuleTyperMatchUnknown(conclusions20))
        }
      }
      case _ => vfail("Can't match " + rule + " against type " + expectedType)
    }
  }

  def matchTypeAgainstRulexSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    irule: IRulexSR):
  (State, IRuleTyperMatchResult[IRulexAR]) = {
    irule match {
      case rule @ EqualsSR(_, _) => {
        matchTypeAgainstEqualsSR(state0, env, conclusions0, expectedType, rule)
      }
      case rule @ OrSR(_) => {
        matchTypeAgainstOrSR(state0, env, conclusions0, expectedType, rule)
      }
      case rule @ ComponentsSR(_, _) => {
        matchTypeAgainstComponentsSR(state0, env, conclusions0, expectedType, rule)
      }
      case rule @ TypedSR(_, _) => {
        matchTypeAgainstTypedSR(state0, env, conclusions0, expectedType, rule)
      }
      case TemplexSR(itemplexST) => {
        matchTypeAgainstTemplexS(state0, env, conclusions0, expectedType, itemplexST) match {
          case (state3, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state3, RuleTyperMatchConflict(conclusions0, "", List(rtmc)))
          case (state3, RuleTyperMatchUnknown(c)) => (state3, RuleTyperMatchUnknown(c))
          case (state3, RuleTyperMatchSuccess(c, templexT)) => (state3, RuleTyperMatchSuccess(c, TemplexAR(templexT)))
        }
      }
      case rule @ CallSR(_, _) => {
        val (state3, result) = matchTypeAgainstCallSR(state0, env, conclusions0, expectedType, rule)
        (state3, result)
      }
    }
  }

  def matchTypeAgainstTypedSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    rule: TypedSR):
  (State, IRuleTyperMatchResult[TemplexAR]) = {
    // If we fail here, that means we didn't take this ITemplataType into account
    // in the main match below.
    expectedType match {
      case CoordTemplataType =>
      case KindTemplataType =>
      case MutabilityTemplataType =>
    }
    // If we fail here, that means we didn't take this ITypeSR into account
    // in the main match below.
    rule.tyype match {
      case CoordTypeSR =>
      case KindTypeSR =>
      case MutabilityTypeSR =>
    }
    (expectedType, rule.tyype) match {
      case (CoordTemplataType, CoordTypeSR) =>
      case (KindTemplataType, KindTypeSR) =>
      case (MutabilityTemplataType, MutabilityTypeSR) =>
      // When you add a case here, make sure you consider all combinations, and
      // add it to the above matches to note that you did.
      case _ => return (state0, RuleTyperMatchConflict(conclusions0, "Type from above (" + expectedType + ") didn't match type from rule (" + rule.tyype + ")", List()))
    }

    val conclusions2 =
      rule.rune match {
        case None => conclusions0
        case Some(rune) => {
          addConclusion(conclusions0, rune, expectedType) match {
            case (imc @ RuleTyperMatchConflict(_, _, _)) => return (state0, RuleTyperMatchConflict(conclusions0, "", List(imc)))
            case (RuleTyperMatchSuccess(conclusions1, ())) => conclusions1
          }
        }
      }

    rule.rune match {
      case Some(rune) => (state0, RuleTyperMatchSuccess(conclusions2, TemplexAR(RuneAT(rune, expectedType))))
      case None => (state0, RuleTyperMatchSuccess(conclusions2, TemplexAR(AnonymousRuneAT(expectedType))))
    }
  }

  def matchTypeAgainstCallSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    rule: CallSR):
  (State, IRuleTyperMatchResult[CallAR]) = {
    val CallSR(name, argRules) = rule
//
//    // We don't do anything with the argRules; we don't evaluate or match them here, see MDMIA.
//    val _ = argRules
//
//    // We could check that the types are good, but we already do that in the evaluate layer.
//    // So... nothing to do here!
//    (RuleTyperMatchContinue(conclusions0))

    name match {
      case "passThroughIfConcrete" => {
        if (expectedType != KindTemplataType) {
          return (state0, RuleTyperMatchConflict(conclusions0, "passThroughIfConcrete returns a kind, but tried to match " + expectedType, List()))
        }
        val List(argRule) = argRules
        matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, argRule) match {
          case (state1, imc @ RuleTyperMatchConflict(_, _, _)) => return (state1, RuleTyperMatchConflict(conclusions0, "Couldn't match against " + name + " argument", List(imc)))
          case (state1, RuleTyperMatchSuccess(conclusions1, ruleT)) => (state1, RuleTyperMatchSuccess(conclusions1, CallAR(name, List(ruleT), KindTemplataType)))
        }
      }
      case "passThroughIfInterface" => {
        if (expectedType != KindTemplataType) {
          return (state0, RuleTyperMatchConflict(conclusions0, "passThroughIfInterface returns a kind, but tried to match " + expectedType, List()))
        }
        val List(argRule) = argRules
        matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, argRule) match {
          case (state1, imc @ RuleTyperMatchConflict(_, _, _)) => return (state1, RuleTyperMatchConflict(conclusions0, "Couldn't match against " + name + " argument", List(imc)))
          case (state1, RuleTyperMatchSuccess(conclusions1, ruleT)) => (state1, RuleTyperMatchSuccess(conclusions1, CallAR(name, List(ruleT), KindTemplataType)))
        }
      }
      case "passThroughIfStruct" => {
        if (expectedType != KindTemplataType) {
          return (state0, RuleTyperMatchConflict(conclusions0, "passThroughIfStruct returns a kind, but tried to match " + expectedType, List()))
        }
        val List(argRule) = argRules
        matchTypeAgainstRulexSR(state0, env, conclusions0, KindTemplataType, argRule) match {
          case (state1, imc @ RuleTyperMatchConflict(_, _, _)) => return (state1, RuleTyperMatchConflict(conclusions0, "Couldn't match against " + name + " argument", List(imc)))
          case (state1, RuleTyperMatchSuccess(conclusions1, ruleT)) => (state1, RuleTyperMatchSuccess(conclusions1, CallAR(name, List(ruleT), KindTemplataType)))
        }
      }
      case _ => vfail()
    }
  }

  def matchTypeAgainstComponentsSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    rule: ComponentsSR):
  (State, IRuleTyperMatchResult[EqualsAR]) = {
    val ComponentsSR(containerTypeAndRuneRuleS, components) = rule

    val (state10, conclusions10, containerTypeAndRuneRuleT) =
      matchTypeAgainstTypedSR(state0, env, conclusions0, expectedType, containerTypeAndRuneRuleS) match {
        case (state1, imc @ RuleTyperMatchConflict(_, _, _)) => return (state1, RuleTyperMatchConflict(conclusions0, "Couldn't match against type/rune of components!", List(imc)))
        case (state1, RuleTyperMatchSuccess(conclusions1, typedAR)) => (state1, conclusions1, typedAR)
      }

    rule.container.tyype match {
      case KindTypeSR => {
        components match {
          case List(mutabilityRuleS) => {
            val (state30, conclusions30, maybeMutabilityRuleT) =
              matchTypeAgainstRulexSR(state10, env, conclusions10, MutabilityTemplataType, mutabilityRuleS) match {
                case (state13, rtmc @ RuleTyperMatchConflict(_, _, _)) => return (state13, RuleTyperMatchConflict(conclusions10, "Couldn't match against mutability rule of kind components rule", List(rtmc)))
                case (state13, RuleTyperMatchUnknown(conclusions13)) => (state13, conclusions13, None)
                case (state13, RuleTyperMatchSuccess(conclusions13, mutabilityRuleT)) => (state13, conclusions13, Some(mutabilityRuleT))
              }
            maybeMutabilityRuleT match {
              case None => (state30, RuleTyperMatchUnknown(conclusions30))
              case Some(mutabilityRuleT) => {
                val componentsRuleT =
                  EqualsAR(
                    containerTypeAndRuneRuleT,
                    ComponentsAR(containerTypeAndRuneRuleT.resultType, List(mutabilityRuleT)))
                (state30, RuleTyperMatchSuccess(conclusions30, componentsRuleT))
              }
            }
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
      case CoordTypeSR => {
        components match {
          case List(ownershipRule, kindRule) => {
            val (state20, conclusions20, maybeOwnershipRuleT) =
              matchTypeAgainstRulexSR(state10, env, conclusions10, OwnershipTemplataType, ownershipRule) match {
                case (state15, imc @ RuleTyperMatchConflict(_, _, _)) => return (state15, RuleTyperMatchConflict(conclusions10, "Failed matching ownership component of coord rule", List(imc)))
                case (state15, RuleTyperMatchSuccess(conclusions15, ownershipRuleT)) => (state15, conclusions15, Some(ownershipRuleT))
              }
            val (state50, conclusions50, maybeKindRuleT) =
              matchTypeAgainstRulexSR(state20, env, conclusions20, KindTemplataType, kindRule) match {
                case (state45, imc @ RuleTyperMatchConflict(_, _, _)) => return (state45, RuleTyperMatchConflict(conclusions10, "Failed matching kind component of coord rule", List(imc)))
                case (state45, RuleTyperMatchSuccess(conclusions45, kindRuleT)) => (state45, conclusions45, Some(kindRuleT))
              }

            (maybeOwnershipRuleT, maybeKindRuleT) match {
              case (Some(ownershipRuleT), Some(kindRuleT)) => {
                val componentsRuleT =
                  EqualsAR(
                    containerTypeAndRuneRuleT,
                    ComponentsAR(containerTypeAndRuneRuleT.resultType, List(ownershipRuleT, kindRuleT)))
                (state50, RuleTyperMatchSuccess(conclusions50, componentsRuleT))
              }
              case (_, _) => (state50, RuleTyperMatchUnknown(conclusions50))
            }
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
    }
  }

  def matchTypeAgainstEqualsSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    rule: EqualsSR):
  (State, IRuleTyperMatchResult[EqualsAR]) = {
    val EqualsSR(left, right) = rule
    matchTypeAgainstRulexSR(state0, env, conclusions0, expectedType, left) match {
      case (state1, imc @ RuleTyperMatchConflict(_, _, _)) => (state1, RuleTyperMatchConflict(conclusions0, "Conflict while evaluating left side of equals!", List(imc)))
      case (state1, RuleTyperMatchSuccess(conclusions1, leftT)) => {
        matchTypeAgainstRulexSR(state1, env, conclusions1, expectedType, right) match {
          case (state2, imc @ RuleTyperMatchConflict(_, _, _)) => (state2, RuleTyperMatchConflict(conclusions1, "Conflict while evaluating right side of equals!", List(imc)))
          case (state2, RuleTyperMatchSuccess(conclusions2, rightT)) => {
            (state2, RuleTyperMatchSuccess(conclusions2, EqualsAR(leftT, rightT)))
          }
        }
      }
    }
  }

  def matchTypeAgainstOrSR(
    state0: State,
    env: Env,
    conclusions0: Conclusions,
    expectedType: ITemplataType,
    rule: OrSR):
  (State, IRuleTyperMatchResult[OrAR]) = {
    val OrSR(possibilities) = rule

    val (state10, conclusions10, possibilitiesT) =
      possibilities.zipWithIndex.foldLeft((state0, conclusions0, List[IRulexAR]()))({
        case ((state1, conclusions1, previousPossibilityRulesT), (possibility, possibilityIndex)) => {
          matchTypeAgainstRulexSR(state1, env, conclusions1, expectedType, possibility) match {
            case (state2, imc @ RuleTyperMatchConflict(_, _, _)) => return (state2, RuleTyperMatchConflict(conclusions1, "Conflict while evaluating alternative " + possibilityIndex, List(imc)))
            case (state2, RuleTyperMatchSuccess(conclusions2, possibilityRuleT)) => {
              (state2, conclusions2, previousPossibilityRulesT :+ possibilityRuleT)
            }
          }
        }
      })
    (state10, RuleTyperMatchSuccess(conclusions10, OrAR(possibilitiesT)))
  }
}
