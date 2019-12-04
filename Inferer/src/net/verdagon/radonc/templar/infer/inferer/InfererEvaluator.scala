package net.verdagon.radonc.templar.infer.inferer

import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar.infer._
import net.verdagon.radonc.templar.templata.{Conversions, ITemplata, _}
import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser.{BorrowP, OwnP, RawP, ShareP}
import net.verdagon.radonc.templar.types.{Kind, _}

import scala.collection.immutable.List

private[infer] trait IInfererEvaluatorDelegate[Env, State] {
  def lookupMemberTypes(
    state0: State,
    kind: Kind,
    // This is here so that the predictor can just give us however many things
    // we expect.
    expectedNumMembers: Int
  ): Option[List[Coord]]

  def getMutability(state0: State, kind: Kind): Mutability

  def getAncestorInterfaceDistance(temputs0: State, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (State, Option[Int])

  def getAncestorInterfaces(temputs0: State, descendantCitizenRef: CitizenRef2):
  (State, Set[InterfaceRef2])

  def getMemberCoords(state: State, structRef: StructRef2): List[Coord]

  def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): (State, Boolean)
}

// Given enough user specified template params and param inputs, we should be able to
// infer everything.
// This class's purpose is to take those things, and see if it can figure out as many
// inferences as possible.

class InfererEvaluator[Env, State](
  templataTemplar: TemplataTemplarInner[Env, State],
  equator: InfererEquator[Env, State],
  delegate: IInfererEvaluatorDelegate[Env, State]) {

  private[infer] def solve(
    env: Env,
    state0: State,
    rulesT: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    directInputs: Map[String, ITemplata],
    paramAtoms: List[AtomSP],
    maybeParamInputs: Option[List[ParamFilter]],
    checkAllRunesPresent: Boolean
  ): (State, IInferSolveResult) = {
    val inferences0 = Inferences(Map(), Map())

    // Feed into the system the things the user already specified.

    val inferences10 =
      directInputs.foldLeft(inferences0)({
        case (inferences1, (rune, directInputTemplata)) => {
          val expectedType = vassertSome(typeByRune.get(rune))
          if (directInputTemplata.tyype != expectedType) {
            return (state0, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, inferences1, "Input for rune " + rune + " has type " + directInputTemplata.tyype + " that doesn't match expected type: " + expectedType, List()))
          }
          inferences1.addConclusion(rune, directInputTemplata)
        }
      })

    // Now we'll try solving a bunch, just to see if there's any contradictions,
    // and if so bail out early.
    val (state15, inferences15) =
      solveUntilSettled(env, state0, rulesT, typeByRune, inferences10) match {
        case (state11, isc @ InferEvaluateConflict(_, _, _)) => return (state11, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, inferences10, "", List(isc)))
        case (state11, InferEvaluateSuccess(inferences11, _, deeplySatisfied)) => {
          // Don't care if its not deeply satisfied, because we'll try solving again soon.
          val (_) = deeplySatisfied

          (state11, inferences11)
        }
      }

    // Now we have template args the user specified, and we know there's no contradictions yet.

    // Next, we'll feed in the arguments that they used in the call.

    val (state20, rules20, inferences20) =
      maybeParamInputs match {
        case None => (state15, rulesT, inferences15)
        case Some(paramInputs) => {
          if (paramAtoms.size != paramInputs.size) {
            return (state15, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, inferences15, "Expected " + paramAtoms.size + " args but got " + paramInputs.size, List()))
          }
          paramAtoms.zip(paramInputs).zipWithIndex.foldLeft((state15, rulesT, inferences15))({
            case ((state16, rules16, inferences16), ((paramAtom, paramFilterInstance), paramIndex)) => {
              addParameterRules(state16, rules16, inferences16, paramAtom, paramFilterInstance, List(paramIndex)) match {
                case (state17, iec @ InferEvaluateConflict(_, _, _)) => {
                  return (state17, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, inferences16, "Failed to add parameter " + paramIndex, List(iec)))
                }
                case (state17, InferEvaluateSuccess(inferences17, rules17, true)) => (state17, rules17, inferences17)
              }
            }
          })
        }
      }

    // Now we'll try solving a bunch, just to see if there's any contradictions,
    // and if so bail.
    val (state25, result25, deeplySatisfied) =
      solveUntilSettled(env, state20, rules20, typeByRune, inferences20) match {
        case (state21, isc @ InferEvaluateConflict(_, _, _)) => return (state21, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, inferences20, "", List(isc)))
        case (state21, InferEvaluateSuccess(inferences11, _, ds)) => (state21, inferences11, ds)
      }

    if (checkAllRunesPresent) {
      val neededRunes = typeByRune.keySet
      if ((neededRunes -- result25.templatasByRune.keySet).nonEmpty) {
        val message = "Not enough to solve! Couldn't figure out: " + (neededRunes -- result25.templatasByRune.keySet)
        return (state25, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, result25, message, List()))
      }
    }
    if (!deeplySatisfied) {
      return (state25, InferSolveFailure(typeByRune, directInputs, maybeParamInputs, result25, "Not deeply satisfied!", List()))
    }
    (state25, InferSolveSuccess(result25))
  }

  // We aren't matching or evaluating anything here, we're just adding more rules depending on what
  // we find in the parameters.
  // Well, I suppose we are adding the argument possibilities (it and superclasses) to the inferences.
  // debt: make a rule that can do that for us. a implicitlyCastableTo rule or something?
  // debt: should we perhaps assemble these rules beforehand? maybe in the scout? it might be a templar concern,
  // is it? if it is, perhaps we can assemble these in some sort of pre-evaluate stage? maybe in the ruletyper,
  // since it's kind of like a compiler?
  // these aren't rules of the function, these are more rules that can help us call them and know what can
  // cast to things that can eventually call it.
  private def addParameterRules(
      state16: State,
      rules16: List[IRulexAR],
      inferences16: Inferences,
      paramAtom: AtomSP,
      paramFilterInstance: ParamFilter,
      paramLocation: List[Int]):
  // TODO: Don't use IInferEvaluateResult for this, because it has a deeplySatisfied member
  // which is n/a for this kind of thing.
  (State, IInferEvaluateResult[List[IRulexAR]]) = {
    val AtomSP(_, patternVirtuality, patternCoordRune, maybePatternDestructure) = paramAtom
    val (state18, rules18, inferences18) =
      paramFilterInstance.tyype.referend match {
        case c: CitizenRef2 => {
          val (state17, ancestorInterfaces) = delegate.getAncestorInterfaces(state16, c)
          val selfAndAncestors = List(c) ++ ancestorInterfaces
          val kindRune = "__SolverKind_" + paramLocation.mkString("_")
          val inferences17 = inferences16.addPossibilities(kindRune, selfAndAncestors.map(KindTemplata))
          val rule =
            EqualsAR(
              TemplexAR(RuneAT(patternCoordRune, CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(
                  TemplexAR(OwnershipAT(Conversions.unevaluateOwnership(paramFilterInstance.tyype.ownership))),
                  TemplexAR(RuneAT(kindRune, KindTemplataType)))))
          (state17, rule :: rules16, inferences17)
        }
        case _ => {
          inferences16.templatasByRune.get(patternCoordRune) match {
            case Some(existingOne) if existingOne != CoordTemplata(paramFilterInstance.tyype) => {
              return (state16, InferEvaluateConflict(inferences16, "Incoming argument type doesnt match already known rune " + paramAtom.coordRune + " value. Had value " + existingOne + " but incoming arg was " + paramFilterInstance.tyype, Nil))
            }
            case _ =>
          }
          val inferences17 = inferences16.addConclusion(patternCoordRune, CoordTemplata(paramFilterInstance.tyype))
          (state16, rules16, inferences17)
        }
      }
    val (state30, rules30, inferences30) =
      (paramFilterInstance.virtuality, patternVirtuality) match {
        case (None, _) => (state18, rules18, inferences18)
        case (Some(Abstract2), Some(AbstractSP)) => (state18, rules18, inferences18)
        case (Some(Override2(superInterface)), Some(OverrideSP(superInterfaceRune))) => {
          // We might already have this superInterface figured out.
          inferences18.templatasByRune.get(superInterfaceRune) match {
            case None => {
              val (state19, ancestorInterfaces) = delegate.getAncestorInterfaces(state18, superInterface)
              val selfAndAncestors = List(superInterface) ++ ancestorInterfaces
              val inferences19 =
                inferences18.addPossibilities(
                  superInterfaceRune,
                  selfAndAncestors.map(KindTemplata))
              (state19, rules18, inferences19)
            }
            case Some(existingInference) => {
              vassert(existingInference == KindTemplata(superInterface))
              (state18, rules18, inferences18)
            }
          }
        }
        case (paramFilterVirtuality, patternVirtuality) => {
          return (
            state18,
            InferEvaluateConflict(
              inferences18,
              "Param filter's virtuality and pattern's virtualities didnt match:\n" + paramFilterVirtuality + "\nand:\n" + patternVirtuality,
              Nil))
        }
      }
    maybePatternDestructure match {
      case None => (state30, InferEvaluateSuccess(inferences30, rules30, true))
      case Some(patternDestructures) => {
        val (inferences32, members) =
          getMemberCoords(state30, inferences30, paramFilterInstance.tyype.referend, patternDestructures.size) match {
            case iec @ InferEvaluateConflict(_, _, _) => return (state30, InferEvaluateConflict(inferences30, "Failed getting members for destructure", List(iec)))
            case InferEvaluateSuccess(inferences31, m, true) => (inferences31, m)
          }
        // Should have already been checked in getMemberCoords
        vassert(members.size == patternDestructures.size)

        val (state40, rules40, inferences40) =
          patternDestructures.zip(members).zipWithIndex.foldLeft((state30, rules30, inferences32))({
            // debt: rename this patternDestructure to something. we need a term for an atom
            // that comes from a destructure.
            // debt: rename atom. probably just to pattern again?
            case ((state33, rules33, inferences33), ((None, _), _)) => (state33, rules33, inferences33)
            case ((state33, rules33, inferences33), ((Some(patternDestructure), member), destructureIndex)) => {
              addParameterRules(state33, rules33, inferences33, patternDestructure, ParamFilter(member, None), paramLocation :+ destructureIndex) match {
                case (state36, iec @ InferEvaluateConflict(_, _, _)) => {
                  return (state36, InferEvaluateConflict(inferences33, "Failed to add parameter " + paramLocation.mkString("/"), List(iec)))
                }
                case (state36, InferEvaluateSuccess(inferences36, rules36, true)) => (state36, rules36, inferences36)
              }
            }
          })
        (state40, InferEvaluateSuccess(inferences40, rules40, true))
      }
    }
  }

  private def solveUntilSettled(
    env: Env,
    state0: State,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    inferences0: Inferences,
  ): (State, IInferEvaluateResult[Unit]) = {
    val (state10, inferences10, deeplySatisfied) =
      rules.foldLeft((state0, inferences0, true))({
        case ((state1, inferences1, deeplySatisfiedSoFar), rule) => {
          evaluateRule(env, state1, inferences1, rule) match {
            case (state2, iec @ InferEvaluateConflict(_, _, _)) => return (state2, InferEvaluateConflict(inferences1, "", List(iec)))
            case (state2, InferEvaluateUnknown(inferences2, thisDeeplySatisfied)) => {
              (state2, inferences2, deeplySatisfiedSoFar && thisDeeplySatisfied)
            }
            case (state2, InferEvaluateSuccess(inferences2, _, thisDeeplySatisfied)) => {
              (state2, inferences2, deeplySatisfiedSoFar && thisDeeplySatisfied)
            }
          }
        }
      })

    if (inferences0 != inferences10) {
      // Things have not settled, we made some sort of progress in this last iteration.
      // Keep going.
      solveUntilSettled(env, state10, rules, typeByRune, inferences10)
    } else {
      // No need to do one last match, because we just did an entire iteration where nothing changed.

      // Now that things are settled, see if there's any possibilities open.
      // Pick any of the possibility sets, and try all of the options.
      inferences10.possibilitiesByRune.keySet.headOption match {
        case Some(rune) => {
          val (inferences11, possibilities) = inferences10.pop(rune)
          println("possibilities to try:\n" + possibilities.mkString("\n"))

          possibilities match {
            case List() => vwat()
            case List(onlyPossibility) => {
              val inferences12 = inferences11.addConclusion(rune, onlyPossibility)
              solveUntilSettled(env, state10, rules, typeByRune, inferences12)
            }
            case _ => {
              val maybeInitialSuccessfulUniverse: Option[IInferEvaluateResult[Unit]] = None
              val (state15, failedUniversesFailures, maybeSuccessfulUniverse) =
                possibilities.foldLeft((state10, List[InferEvaluateConflict[Unit]](), maybeInitialSuccessfulUniverse))({
                  case ((state11, previousFailures, Some(iss @ InferEvaluateSuccess(_, _, _))), _) => {
                    (state11, previousFailures, Some(iss))
                  }
                  case ((state11, previousFailures, None), possibility) => {
                    println("trying universe for " + rune + ": " + possibility)
                    val inferences12 = inferences11.addConclusion(rune, possibility)
                    solveUntilSettled(env, state11, rules, typeByRune, inferences12) match {
                      case (state12, isf @ InferEvaluateConflict(_, _, _)) => {
                        println("it didnt work!")
                        (state12, isf :: previousFailures, None)
                      }
                      case (state12, iss @ InferEvaluateSuccess(_, _, _)) => {
                        println("it worked!")
                        (state12, List(), Some(iss))
                      }
                    }
                  }
                })
              maybeSuccessfulUniverse match {
                case None => (state15, InferEvaluateConflict(inferences10, "No options for " + rune + " worked!", failedUniversesFailures))
                case Some(successfulUniverse) => (state15, successfulUniverse)
              }
            }
          }
        }
        case None => {
          // No possibilities, we have nothing left to explore, bail!
          (state10, InferEvaluateSuccess(inferences10, Unit, deeplySatisfied))
        }
      }

    }
  }

  private[infer] def evaluateRule(
    env: Env,
    state0: State,
    inferences0: Inferences,
    rule: IRulexAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    rule match {
      case r @ EqualsAR(_, _) => evaluateEqualsRule(env, state0, inferences0, r)
      case r @ IsaAR(_, _) => evaluateIsaRule(env, state0, inferences0, r)
      case r @ OrAR(_) => evaluateOrRule(env, state0, inferences0, r)
      case r @ ComponentsAR(_, _) => evaluateComponentsRule(env, state0, inferences0, r)
      case TemplexAR(templex) => evaluateTemplex(env, state0, inferences0, templex)
      case r @ CallAR(_, _, _) => evaluateRuleCall(env, state0, inferences0, r)
    }
  }

  private[infer] def evaluateRules(
    env: Env,
    state0: State,
    inferences0: Inferences,
    rules: List[IRulexAR],
  ): (State, IInferEvaluateResult[List[ITemplata]]) = {
    val initialResult: IInferEvaluateResult[List[ITemplata]] =
      InferEvaluateSuccess(inferences0, List(), true)
    rules.zipWithIndex.foldLeft((state0, initialResult))({
      case ((state6, InferEvaluateUnknown(inferences6, deeplySatisfiedSoFar)), (rule, index)) => {
        evaluateRule(env, state6, inferences6, rule) match {
          case (state7, iec @ InferEvaluateConflict(_, _, _)) => {
            return (state7, InferEvaluateConflict(inferences6, "Failed evaluating rule index " + index, List(iec)))
          }
          case (state7, InferEvaluateUnknown(inferences7, deeplySatisfied)) => {
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
          case (state7, InferEvaluateSuccess(inferences7, result, deeplySatisfied)) => {
            // Throw it away; since one is unknown theyre all unknown
            val (_) = result
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
        }
      }
      case ((state6, InferEvaluateSuccess(inferences6, resultsSoFar, deeplySatisfiedSoFar)), (rule, index)) => {
        evaluateRule(env, state6, inferences6, rule) match {
          case (state7, iec @ InferEvaluateConflict(_, _, _)) => {
            return (state7, InferEvaluateConflict(inferences6, "Failed evaluating rule index " + index, List(iec)))
          }
          case (state7, InferEvaluateUnknown(inferences7, deeplySatisfied)) => {
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
          case (state7, InferEvaluateSuccess(inferences7, result, deeplySatisfied)) => {
            (state7, InferEvaluateSuccess(inferences7, resultsSoFar :+ result, deeplySatisfiedSoFar && deeplySatisfied))
          }
        }
      }
    })
  }

  private[infer] def evaluateRuleCall(
    env: Env,
    state0: State,
    inferences0: Inferences,
    ruleCall: CallAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    val CallAR(name, argumentRules, resultType) = ruleCall

    name match {
//      case "ownership" => {
//        checkArgs(List(CoordTypeTR), argTemplatas)
//        val List(CoordTemplata(coord)) = argTemplatas
//        (state1, InferEvaluateSuccess(inferences1, OwnershipTemplata(coord.ownership)))
//      }
//      case "mutability" => {
//        checkArgs(List(KindTypeTR), argTemplatas)
//        val List(KindTemplata(kind)) = argTemplatas
//        val mutability = delegate.getMutability(state1, kind)
//        (state1, InferEvaluateSuccess(inferences1, MutabilityTemplata(mutability)))
//      }
      case "toRef" => {
        val (state1, inferences1, argTemplatas, deeplySatisfied) =
          evaluateRules(env, state0, inferences0, argumentRules) match {
            case (state0b, iec @ InferEvaluateConflict(_, _, _)) => {
              return (state0b, InferEvaluateConflict(inferences0, "Failed evaluating CallAR arguments", List(iec)))
            }
            case (state0b, InferEvaluateUnknown(inferences0b, argDeeplySatisfied)) => {
              // Doesn't matter if the arg is deeply satisfied because this rule itself is not satisfied.
              val (_) = argDeeplySatisfied
              val deeplySatisfied = false
              println("toRef unsatisfied")
              return (state0b, InferEvaluateUnknown(inferences0b, deeplySatisfied))
            }
            case (state0b, InferEvaluateSuccess(inferences0b, arguments, ds)) => {
              (state0b, inferences0b, arguments, ds)
            }
          }

        val List(KindTemplata(kind)) = argTemplatas
        val coord = templataTemplar.pointifyReferend(state1, kind, Own)
        (state1, InferEvaluateSuccess(inferences1, CoordTemplata(coord), deeplySatisfied))
      }
      case "passThroughIfConcrete" => {
        val (state1, inferences1, argTemplatas, deeplySatisfied) =
          evaluateRules(env, state0, inferences0, argumentRules) match {
            case (state0b, iec @ InferEvaluateConflict(_, _, _)) => {
              return (state0b, InferEvaluateConflict(inferences0, "Failed evaluating CallAR arguments", List(iec)))
            }
            case (state0b, InferEvaluateUnknown(inferences0b, argDeeplySatisfied)) => {
              // Doesn't matter if the arg is deeply satisfied because this rule itself is not satisfied.
              val (_) = argDeeplySatisfied
              val deeplySatisfied = false
              println("passThroughIfConcrete unsatisfied")
              return (state0b, InferEvaluateUnknown(inferences0b, deeplySatisfied))
            }
            case (state0b, InferEvaluateSuccess(inferences0b, arguments, ds)) => {
              (state0b, inferences0b, arguments, ds)
            }
          }
        val List(templata) = argTemplatas
        templata match {
          case k @ KindTemplata(StructRef2(_) | PackT2(_, _) | TupleT2(_, _) | ArraySequenceT2(_, _) | UnknownSizeArrayT2(_)) => {
            (state1, InferEvaluateSuccess(inferences1, k, deeplySatisfied))
          }
          case _ => return (state1, InferEvaluateConflict(inferences0, "passThroughIfConcrete expected concrete kind, but got " + templata, List()))
        }
      }
      case "passThroughIfInterface" => {
        val (state1, inferences1, argTemplatas, deeplySatisfied) =
          evaluateRules(env, state0, inferences0, argumentRules) match {
            case (state0b, iec @ InferEvaluateConflict(_, _, _)) => {
              return (state0b, InferEvaluateConflict(inferences0, "Failed evaluating CallAR arguments", List(iec)))
            }
            case (state0b, InferEvaluateUnknown(inferences0b, argDeeplySatisfied)) => {
              // Doesn't matter if the arg is deeply satisfied because this rule itself is not satisfied.
              val (_) = argDeeplySatisfied
              val deeplySatisfied = false
              println("passThroughIfInterface unsatisfied")
              return (state0b, InferEvaluateUnknown(inferences0b, deeplySatisfied))
            }
            case (state0b, InferEvaluateSuccess(inferences0b, arguments, ds)) => {
              (state0b, inferences0b, arguments, ds)
            }
          }
        val List(templata) = argTemplatas
        templata match {
          case k @ KindTemplata(InterfaceRef2(_)) => {
            (state1, InferEvaluateSuccess(inferences1, k, deeplySatisfied))
          }
          case _ => return (state1, InferEvaluateConflict(inferences0, "passThroughIfInterface expected interface kind, but got " + templata, List()))
        }
      }
      case "passThroughIfStruct" => {
        val (state1, inferences1, argTemplatas, deeplySatisfied) =
          evaluateRules(env, state0, inferences0, argumentRules) match {
            case (state0b, iec @ InferEvaluateConflict(_, _, _)) => {
              return (state0b, InferEvaluateConflict(inferences0, "Failed evaluating CallAR arguments", List(iec)))
            }
            case (state0b, InferEvaluateUnknown(inferences0b, argDeeplySatisfied)) => {
              // Doesn't matter if the arg is deeply satisfied because this rule itself is not satisfied.
              val (_) = argDeeplySatisfied
              val deeplySatisfied = false
              println("passThroughIfStruct unsatisfied")
              return (state0b, InferEvaluateUnknown(inferences0b, deeplySatisfied))
            }
            case (state0b, InferEvaluateSuccess(inferences0b, arguments, ds)) => {
              (state0b, inferences0b, arguments, ds)
            }
          }
        val List(templata) = argTemplatas
        templata match {
          case k @ KindTemplata(InterfaceRef2(_)) => {
            (state1, InferEvaluateSuccess(inferences1, k, deeplySatisfied))
          }
          case _ => return (state1, InferEvaluateConflict(inferences0, "passThroughIfInterface expected interface kind, but got " + templata, List()))
        }
      }
      case _ => vfail("Unknown function \"" + name + "\"!");
    }
  }

  private[infer] def evaluateTemplex(
    env: Env,
    state0: State,
    inferences0: Inferences,
    ruleTemplex: ITemplexA
  ): (State, IInferEvaluateResult[ITemplata]) = {
    ruleTemplex match {
      case IntAT(value) => {
        (state0, InferEvaluateSuccess(inferences0, IntegerTemplata(value), true))
      }
      case BoolAT(value) => {
        (state0, InferEvaluateSuccess(inferences0, BooleanTemplata(value), true))
      }
      case MutabilityAT(mutability) => {
        (state0, InferEvaluateSuccess(inferences0, MutabilityTemplata(Conversions.evaluateMutability(mutability)), true))
      }
      case PermissionAT(permission) => {
        (state0, InferEvaluateSuccess(inferences0, PermissionTemplata(Conversions.evaluatePermission(permission)), true))
      }
      case LocationAT(location) => {
        (state0, InferEvaluateSuccess(inferences0, LocationTemplata(Conversions.evaluateLocation(location)), true))
      }
      case OwnershipAT(ownership) => {
        (state0, InferEvaluateSuccess(inferences0, OwnershipTemplata(Conversions.evaluateOwnership(ownership)), true))
      }
      case VariabilityAT(variability) => {
        (state0, InferEvaluateSuccess(inferences0, VariabilityTemplata(Conversions.evaluateVariability(variability)), true))
      }
      case NameAT(name, expectedType) => {
        val (state1, templata) =
          templataTemplar.lookupTemplata(env, state0, name, expectedType)
        (state1, InferEvaluateSuccess(inferences0, templata, true))
      }
      case AnonymousRuneAT(_) => {
        // See ARCDS for why anonymous runes are considered deeply satisfied.
        val deeplySatisfied = true

        (state0, InferEvaluateUnknown(inferences0, deeplySatisfied))
      }
      case RuneAT(rune, expectedType) => {
        inferences0.templatasByRune.get(rune) match {
          case Some(templata) => {
            if (templata.tyype != expectedType) {
              return (state0, InferEvaluateConflict(inferences0, "Rune " + rune + " is of type " + expectedType + ", but it received a " + templata.tyype + ", specifically " + templata, List()))
            }
            (state0, InferEvaluateSuccess(inferences0, templata, true))
          }
          case None => {
            println("RuneAT unsatisfied")
            (state0, InferEvaluateUnknown(inferences0, false))
          }
        }
      }
      case OwnershippedAT(targetOwnership, innerKindRule) => {
        evaluateTemplex(env, state0, inferences0, innerKindRule) match {
          case (state1, iec @ InferEvaluateConflict(_, _, _)) => return (state1, InferEvaluateConflict(inferences0, "bogglewogget", List(iec)))
          case (state1, InferEvaluateUnknown(inferences1, innerCoordDeeplySatisfied)) => {
            // If we don't know the inner coord, we can't verify that the ownership is compatible with the inner kind.
            // For example, we can't do a borrow of something that's already a borrow or a weak.
            val (_) = innerCoordDeeplySatisfied
            val deeplySatisfied = false
            println("OwnershippedAT unsatisfied")

            (state1, InferEvaluateUnknown(inferences1, deeplySatisfied))
          }
          case (state1, InferEvaluateSuccess(inferences1, CoordTemplata(Coord(innerCoordOwnership, innerCoordKind)), innerCoordDeeplySatisfied)) => {

            val resultingOwnership =
              (innerCoordOwnership, targetOwnership) match {
                case (Own, ShareP) => return (state1, InferEvaluateConflict(inferences1, "Expected a share, but was an own!", List()))
                case (Own, OwnP) => Own // No change, allow it
                case (Own, BorrowP) => Borrow // Can borrow an own, allow it
                case (Own, RawP) => return (state1, InferEvaluateConflict(inferences1, "Expected a raw, but was an own!", List()))
                case (Borrow, ShareP) => return (state1, InferEvaluateConflict(inferences1, "Expected a share, but was a borrow!", List()))
                case (Borrow, OwnP) => Own
                case (Borrow, BorrowP) => Borrow // No change, allow it
                case (Borrow, RawP) => return (state1, InferEvaluateConflict(inferences1, "Expected a raw, but was a borrow!", List()))
                case (Share, OwnP) => Share // Can own a share, just becomes another share.
                case (Share, BorrowP) => Share // Can borrow a share, just becomes another share.
                case (Share, ShareP) => Share // No change, allow it
                case (Share, RawP) => return (state1, InferEvaluateConflict(inferences1, "Expected a raw, but was a share!", List()))
                case (Raw, OwnP) => Raw // Can own a raw, just becomes another raw.
                case (Raw, BorrowP) => Raw // Can borrow a raw, just becomes another raw.
                case (Raw, ShareP) => {
                  return (state1, InferEvaluateConflict(inferences1, "Expected a share, but was a raw!", List()))
                }
                case (Raw, RawP) => Raw // No change, allow it
              }

            // If we got here then the ownership and mutability were compatible.
            val satisfied = true
            val deeplySatisfied = innerCoordDeeplySatisfied && satisfied

            (state1, InferEvaluateSuccess(inferences1, CoordTemplata(Coord(resultingOwnership, innerCoordKind)), deeplySatisfied))
          }
        }
      }
      case CallAT(templateRule, listOfMaybeArgRules, callResultType) => {

        // it should be a template that results in a `tyype`

        val (state2, inferences2, maybeTemplateTemplata, templateDeeplySatisfied) =
          evaluateTemplex(env, state0, inferences0, templateRule) match {
            case (state1, iec @ InferEvaluateConflict(_, _, _)) => return (state1, InferEvaluateConflict(inferences0, "bogglewogget", List(iec)))
            case (state1, InferEvaluateUnknown(inferences1, ds)) => (state1, inferences1, None, ds)
            case (state1, InferEvaluateSuccess(inferences1, templata, ds)) => (state1, inferences1, Some(templata), ds)
          }

        val (state10, inferences10, maybeArgTemplatas, argsDeeplySatisfied) =
          evaluateTemplexes(env, state2, inferences2, listOfMaybeArgRules) match {
            case (state3, iec @ InferEvaluateConflict(_, _, _)) => {
              return (state3, InferEvaluateConflict(inferences2, "Failed to evaluate CallAT arguments", List(iec)))
            }
            case (state3, InferEvaluateUnknown(inferences3, ds)) => {
              (state3, inferences3, None, ds)
            }
            case (state3, InferEvaluateSuccess(inferences3, argTemplatas, ds)) => {
              (state3, inferences3, Some(argTemplatas), ds)
            }
          }

        (maybeTemplateTemplata, maybeArgTemplatas) match {
          case (None, _) => {
            println("CallAT 1 unsatisfied")
            (state10, InferEvaluateUnknown(inferences10, false))
          }
          case (_, None) => {
            println("CallAT 2 unsatisfied")
            (state10, InferEvaluateUnknown(inferences10, false))
          }
          case (Some(it @ InterfaceTemplata(_, _)), Some(listOfArgTemplatas)) => {
            val (state11, result) =
              templataTemplar.evaluateInterfaceTemplata(state10, it, listOfArgTemplatas, callResultType)
            (state11, InferEvaluateSuccess(inferences10, result, templateDeeplySatisfied && argsDeeplySatisfied))
          }
          case (Some(st @ StructTemplata(_, _)), Some(listOfArgTemplatas)) => {
            val (state11, result) =
              templataTemplar.evaluateStructTemplata(state10, st, listOfArgTemplatas, callResultType)
            (state11, InferEvaluateSuccess(inferences10, result, templateDeeplySatisfied && argsDeeplySatisfied))
          }
          case (Some(btt @ ArrayTemplateTemplata()), Some(listOfArgTemplatas)) => {
            val (state11, result) =
              templataTemplar.evaluateBuiltinTemplateTemplata(state10, btt, listOfArgTemplatas, callResultType)
            (state11, InferEvaluateSuccess(inferences10, result, templateDeeplySatisfied && argsDeeplySatisfied))
          }
          case (_, _) => {
            vcurious() // it feels sfinae-ey
            (state10, InferEvaluateUnknown(inferences10, vimpl()))
          }
        }
      }
      case PrototypeAT(_, _, _) => {
        vfail("Unimplemented")
      }
      case PackAT(memberTemplexes, resultType) => {
        evaluateTemplexes(env, state0, inferences0, memberTemplexes) match {
          case (state3, iec @ InferEvaluateConflict(_, _, _)) => {
            return (state3, InferEvaluateConflict(inferences0, "Failed to evaluate CallAT arguments", List(iec)))
          }
          case (state3, InferEvaluateUnknown(inferences3, deeplySatisfied)) => {
            (state3, InferEvaluateUnknown(inferences3, deeplySatisfied))
          }
          case (state3, InferEvaluateSuccess(inferences3, memberTemplatas, deeplySatisfied)) => {
            val memberCoords = memberTemplatas.collect({ case CoordTemplata(coord) => coord })
            if (memberCoords.size != memberTemplatas.size) {
              vfail("Packs can only take coords!")
            }

            val (state2, packTemplata) = templataTemplar.getPackKind(env, state3, memberCoords, resultType)
            (state2, InferEvaluateSuccess(inferences3, packTemplata, deeplySatisfied))
          }
        }
      }
      case RepeaterSequenceAT(mutabilityTemplex, sizeTemplex, elementTemplex, resultType) => {
        val (state9, inferences9, maybeMutability, mutabilityDeeplySatisfied) =
          evaluateTemplex(env, state0, inferences0, mutabilityTemplex) match {
            case (state3, iec @ InferEvaluateConflict(_, _, _)) => return (state3, InferEvaluateConflict(inferences0, "Failed to evaluate size", List(iec)))
            case (state3, InferEvaluateUnknown(inferences3, ds)) => (state3, inferences3, None, ds)
            case (state3, InferEvaluateSuccess(inferences3, MutabilityTemplata(mutability), ds)) => (state3, inferences3, Some(mutability), ds)
            case (state3, InferEvaluateSuccess(inferences13, notInt, _)) => return (state3, InferEvaluateConflict(inferences13, "Size isn't an int: " + notInt, Nil))
          }
        val (state10, inferences10) = (state9, inferences9)
        val (state19, inferences19, maybeSize, sizeDeeplySatisfied) =
          evaluateTemplex(env, state10, inferences10, sizeTemplex) match {
            case (state13, iec @ InferEvaluateConflict(_, _, _)) => return (state13, InferEvaluateConflict(inferences10, "Failed to evaluate element", List(iec)))
            case (state13, InferEvaluateUnknown(inferences13, ds)) => (state13, inferences13, None, ds)
            case (state13, InferEvaluateSuccess(inferences13, IntegerTemplata(size), ds)) => (state13, inferences13, Some(size), ds)
            case (state13, InferEvaluateSuccess(inferences13, notCoord, _)) => return (state13, InferEvaluateConflict(inferences13, "Element isn't a coord: " + notCoord, Nil))
          }
        val (state20, inferences20) = (state19, inferences19)
        val (state29, inferences29, maybeElement, elementDeeplySatisfied) =
          evaluateTemplex(env, state20, inferences20, elementTemplex) match {
            case (state23, iec @ InferEvaluateConflict(_, _, _)) => return (state23, InferEvaluateConflict(inferences20, "Failed to evaluate element", List(iec)))
            case (state23, InferEvaluateUnknown(inferences23, ds)) => (state23, inferences23, None, ds)
            case (state23, InferEvaluateSuccess(inferences23, CoordTemplata(coord), ds)) => (state23, inferences23, Some(coord), ds)
            case (state23, InferEvaluateSuccess(inferences23, notCoord, _)) => return (state23, InferEvaluateConflict(inferences23, "Element isn't a coord: " + notCoord, Nil))
          }
        val (state30, inferences30) = (state29, inferences29)

        (maybeMutability, maybeSize, maybeElement) match {
          case (Some(mutability), Some(size), Some(element)) => {
            val (state40, tuple) =
              templataTemplar.getArraySequenceKind(env, state30, mutability, size, element, resultType)
            (state40, InferEvaluateSuccess(inferences30, tuple, mutabilityDeeplySatisfied && sizeDeeplySatisfied && elementDeeplySatisfied))
          }
          case _ => {
            // Not satisfied because there's an implicit constraint that these things together make up a valid repeater sequence.
            val deeplySatisfied = false
            println("Repeater unsatisfied")
            (state30, InferEvaluateUnknown(inferences30, deeplySatisfied))
          }
        }
      }
      case ManualSequenceAT(_, _) => {
        vfail("Unimplemented")
      }
    }
  }

  private[infer] def evaluateTemplexes(
    env: Env,
    state0: State,
    inferences0: Inferences,
    templexes: List[ITemplexA]):
  (State, IInferEvaluateResult[List[ITemplata]]) = {
    val initialFoldyThing: IInferEvaluateResult[List[ITemplata]] =
      InferEvaluateSuccess(inferences0, List[ITemplata](), true)
    templexes.zipWithIndex.foldLeft((state0, initialFoldyThing))({
      case ((state6, InferEvaluateSuccess(inferences6, resultsSoFar, deeplySatisfiedSoFar)), (maybeArgRule, index)) => {
        evaluateTemplex(env, state6, inferences6, maybeArgRule) match {
          case (state7, iec @ InferEvaluateConflict(_, _, _)) => {
            return (state7, InferEvaluateConflict[List[ITemplata]](inferences6, "Failed to evaluate templex " + index, List(iec)))
          }
          case (state7, InferEvaluateUnknown(inferences7, deeplySatisfied)) => {
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
          case (state7, InferEvaluateSuccess(inferences7, result, deeplySatisfied)) => {
            (state7, InferEvaluateSuccess(inferences7, resultsSoFar :+ result, deeplySatisfiedSoFar && deeplySatisfied))
          }
        }
      }
      case ((state6, InferEvaluateUnknown(inferences6, deeplySatisfiedSoFar)), (maybeArgRule, index)) => {
        evaluateTemplex(env, state6, inferences6, maybeArgRule) match {
          case (state7, iec @ InferEvaluateConflict(_, _, _)) => {
            return (state7, InferEvaluateConflict[List[ITemplata]](inferences6, "Failed to evaluate templex " + index, List(iec)))
          }
          case (state7, InferEvaluateUnknown(inferences7, deeplySatisfied)) => {
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
          case (state7, InferEvaluateSuccess(inferences7, result, deeplySatisfied)) => {
            // Throw it away; since there was one unknown the entire thing's unknown.
            val (_) = result
            (state7, InferEvaluateUnknown(inferences7, deeplySatisfiedSoFar && deeplySatisfied))
          }
        }
      }
    })
  }

  private[infer] def evaluateEqualsRule(
    env: Env,
    state0: State,
    inferences0: Inferences,
    rule: EqualsAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    val EqualsAR(leftRule, rightRule) = rule

    evaluateRule(env, state0, inferences0, leftRule) match {
      case (state1, iec @ InferEvaluateConflict(_, _, _)) => return (state1, InferEvaluateConflict(inferences0, "Failed evaluating left rule!", List(iec)))
      case (state1, InferEvaluateUnknown(inferences1, leftEvalDeeplySatisfied)) => {
        evaluateRule(env, state1, inferences1, rightRule) match {
          case (state2, iec @ InferEvaluateConflict(_, _, _)) => return (state2, InferEvaluateConflict(inferences1, "Failed evaluating right rule!", List(iec)))
          case (state2, InferEvaluateUnknown(inferences2, rightDeeplySatisfied)) => {
            // Both sides are unknown, so return an unknown.

            // Doesn't matter if either side was deeply satisfied, because this equals itself isn't satisfied.
            val (_) = leftEvalDeeplySatisfied
            val (__) = rightDeeplySatisfied
            val deeplySatisfied = false
            println("Equals 1 unsatisfied")

            (state2, InferEvaluateUnknown(inferences2, deeplySatisfied))
          }
          case (state2, InferEvaluateSuccess(inferences2, rightTemplata, rightDeeplySatisfied)) => {
            // Left is unknown, but right is known. Use the thing from the right
            // and match it against the left.
            val (state3, maybeResult3) =
              makeMatcher().matchTemplataAgainstRulexTR(
                env, state2, inferences2, rightTemplata, leftRule)
            maybeResult3 match {
              case imc @ InferMatchConflict(_, _, _) => {
                // None from the match means something conflicted, bail!
                return (state3, InferEvaluateConflict(inferences2, "Failed to match known right against unknown left!", List(imc)))
              }
              case InferMatchSuccess(inferences3, leftMatchDeeplySatisfied) => {
                // Doesn't matter if the left was deeply satisfied in eval, because it's more likely
                // that it was satisfied in the match.
                val (_) = leftEvalDeeplySatisfied
                val deeplySatisfied = leftMatchDeeplySatisfied && rightDeeplySatisfied

                (state3, InferEvaluateSuccess(inferences3, rightTemplata, deeplySatisfied))
              }
            }
          }
        }
      }
      case (state1, InferEvaluateSuccess(inferences1, leftTemplata, leftDeeplySatisfied)) => {
        evaluateRule(env, state1, inferences1, rightRule) match {
          case (state2, iec @ InferEvaluateConflict(_, _, _)) => return (state2, InferEvaluateConflict(inferences1, "Failed evaluating right rule!", List(iec)))
          case (state2, InferEvaluateUnknown(inferences2, rightEvalDeeplySatisfied)) => {
            // We don't care about the eval being deeply satisfied because we'll be matching it shortly.
            val (_) = rightEvalDeeplySatisfied

            // Right is unknown, but left is known. Use the thing from the left
            // and match it against the right.
            val (state3, maybeInferences3) =
              makeMatcher().matchTemplataAgainstRulexTR(
                env, state2, inferences2, leftTemplata, rightRule)
            maybeInferences3 match {
              case imc @ InferMatchConflict(_, _, _) => {
                // None from the match means something conflicted, bail!
                return (state3, InferEvaluateConflict(inferences2, "Failed to match known left against unknown right!", List(imc)))
              }
              case InferMatchSuccess(inferences3, rightMatchDeeplySatisfied) => {
                (state3, InferEvaluateSuccess(inferences3, leftTemplata, leftDeeplySatisfied && rightMatchDeeplySatisfied))
              }
            }
          }
          case (state2, InferEvaluateSuccess(inferences2, rightTemplata, rightDeeplySatisfied)) => {
            // Both sides are known. Make sure they're equal.
            val (state3, equal) =
              equator.equals(state2, leftTemplata, rightTemplata, leftRule.resultType)
            if (equal) {
              // Could return either, arbitrarily choosing left
              (state3, InferEvaluateSuccess(inferences2, leftTemplata, leftDeeplySatisfied && rightDeeplySatisfied))
            } else {
              (state3, InferEvaluateConflict(inferences2, s"Sides aren't equal!\nLeft:  ${leftTemplata}\nRight: ${rightTemplata}", Nil))
            }
          }
        }
      }
    }
  }

  private[infer] def evaluateIsaRule(
    env: Env,
    state0: State,
    inferences0: Inferences,
    rule: IsaAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    val IsaAR(subRule, superRule) = rule

    val (state10, inferences10, maybeSub, subDeeplySatisfied) =
      evaluateRule(env, state0, inferences0, subRule) match {
        case (state1, iec @ InferEvaluateConflict(_, _, _)) => return (state1, InferEvaluateConflict(inferences0, "Failed evaluating sub rule!", List(iec)))
        case (state1, InferEvaluateUnknown(inferences1, ds)) => (state1, inferences1, None, ds)
        case (state1, InferEvaluateSuccess(inferences1, subTemplata, ds)) => (state1, inferences1, Some(subTemplata), ds)
      }

    val (state20, inferences20, maybeConcept, conceptDeeplySatisfied) =
      evaluateRule(env, state10, inferences10, superRule) match {
        case (state11, iec @ InferEvaluateConflict(_, _, _)) => return (state11, InferEvaluateConflict(inferences10, "Failed evaluating concept rule!", List(iec)))
        case (state11, InferEvaluateUnknown(inferences1, ds)) => (state11, inferences1, None, ds)
        case (state11, InferEvaluateSuccess(inferences1, subTemplata, ds)) => (state11, inferences1, Some(subTemplata), ds)
      }

    (maybeSub, maybeConcept) match {
      case (Some(KindTemplata(sub : CitizenRef2)), Some(KindTemplata(suuper : InterfaceRef2))) => {
        val (state30, supers) = delegate.getAncestorInterfaces(state20, sub)

        if (supers.contains(suuper)) {
          val isaSatisfied = true
          val deeplySatisfied = subDeeplySatisfied && conceptDeeplySatisfied && isaSatisfied
          (state20, InferEvaluateSuccess(inferences20, KindTemplata(sub), deeplySatisfied))
        } else {
          return (state30, InferEvaluateConflict(inferences10, "Isa failed!\nSub: " + sub + "\nSuper: " + suuper, List()))
        }
      }
      case (Some(_), Some(_)) => vfail()
      case _ => {
        println("conforms unsatisfied")
        (state20, InferEvaluateUnknown(inferences20, false))
      }
    }
  }


  private[infer] def evaluateOrRule(
    env: Env,
    state0: State,
    inferences0: Inferences,
    rule: OrAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    // We don't actually evaluate Ors, we only match against them.
    // For this reason, it doesn't make sense to have an or at the top level.
    // We just return unknown since we can't know which of the branches we're using.

    // We can't satisfy Or rules with evaluating, only with matching.
    val deeplySatisfied = false
    println("or unsatisfied")

    (state0, InferEvaluateUnknown(inferences0, deeplySatisfied))
  }

  private[infer] def evaluateComponentsRule(
    env: Env,
    state0: State,
    inferencesA: Inferences,
    rule: ComponentsAR
  ): (State, IInferEvaluateResult[ITemplata]) = {
    val ComponentsAR(_, components) = rule

    // We don't have a value from the rune, we just have the type. Try to evaluate the components.
    rule.tyype match {
      case KindTemplataType => {
        evaluateKindComponents(env, state0, inferencesA, components) match {
          case (state3, iec @ InferEvaluateConflict(_, _, _)) => return (state3, InferEvaluateConflict(inferencesA, "Failed evaluating kind components!", List(iec)))
          case (state3, InferEvaluateUnknown(inferencesB, ds)) => (state3, InferEvaluateUnknown(inferencesB, ds))
          case (state3, InferEvaluateSuccess(inferencesB, templataFromRune, ds)) => (state3, InferEvaluateSuccess(inferencesB, templataFromRune, ds))
        }
      }
      case CoordTemplataType => {
        evaluateCoordComponents(env, state0, inferencesA, components) match {
          case (state3, iec @ InferEvaluateConflict(_, _, _)) => return (state3, InferEvaluateConflict(inferencesA, "Failed evaluating coord components!", List(iec)))
          case (state3, InferEvaluateUnknown(inferencesB, ds)) => (state3, InferEvaluateUnknown(inferencesB, ds))
          case (state3, InferEvaluateSuccess(inferencesB, templataFromRune, ds)) => (state3, InferEvaluateSuccess(inferencesB, templataFromRune, ds))
        }
      }
      case _ => vfail("Can only destructure coords and kinds!")
    }
  }

  private def evaluateCoordComponents(
    env: Env,
    state0: State,
    inferencesC: Inferences,
    components: List[IRulexAR]):
  (State, IInferEvaluateResult[ITemplata]) = {
    // Now we're going to try and evaluate all the components.
    // At the end, if we have values for every component, then we'll
    // assemble a shiny new coord out of them!
    components match {
      case List(ownershipRule, kindRule) => {
        val (state10, inferences10, maybeOwnership, ownershipDeeplySatisfied) =
          evaluateRule(env, state0, inferencesC, ownershipRule) match {
            case (state3, iec@InferEvaluateConflict(_, _, _)) => return (state3, InferEvaluateConflict(inferencesC, "floop", List(iec)))
            case (state3, InferEvaluateUnknown(inferences1, ds)) => (state3, inferences1, None, ds)
            case (state3, InferEvaluateSuccess(inferences1, templata, ds)) => {
              templata match {
                case OwnershipTemplata(ownership) => (state3, inferences1, Some(ownership), ds)
                case _ => vfail("First component of Coord must be an ownership!")
              }
            }
          }
        val (state40, inferences40, maybeKind, kindDeeplySatisfied) =
          evaluateRule(env, state10, inferences10, kindRule) match {
            case (state31, iec@InferEvaluateConflict(_, _, _)) => return (state31, InferEvaluateConflict(inferences10, "sparklebark", List(iec)))
            case (state31, InferEvaluateUnknown(inferences31, ds)) => (state31, inferences31, None, ds)
            case (state31, InferEvaluateSuccess(inferences31, templata, ds)) => {
              templata match {
                case KindTemplata(kind) => (state31, inferences31, Some(kind), ds)
                case _ => vfail("Fourth component of Coord must be a kind!")
              }
            }
          }
        val deeplySatisfied = ownershipDeeplySatisfied && kindDeeplySatisfied
        (maybeOwnership, maybeKind) match {
          case (Some(ownership), Some(kind)) => {
            val newOwnership =
              if (kind == Void2()) Raw
              else if (delegate.getMutability(state40, kind) == Immutable) Share
              else ownership
            (state40, InferEvaluateSuccess(inferences40, CoordTemplata(Coord(newOwnership, kind)), deeplySatisfied))
          }
          case _ => {
            // deeplySatisfied can still be true even if the result is unknown, see IEUNDS.
            (state40, InferEvaluateUnknown(inferences40, deeplySatisfied))
          }
        }
      }
      case _ => vfail("Coords must have 4 components")
    }
  }

  private def evaluateKindComponents(
      env: Env,
      state0: State,
      inferencesC: Inferences,
      components: List[IRulexAR],
      ):
  (State, IInferEvaluateResult[ITemplata]) = {
    val List(mutabilityRule) = components
    evaluateRule(env, state0, inferencesC, mutabilityRule) match {
      case (state3, iec@InferEvaluateConflict(_, _, _)) => (state3, InferEvaluateConflict(inferencesC, "klippityklap", List(iec)))
      case (state3, InferEvaluateUnknown(inferences6, ds)) => (state3, InferEvaluateUnknown(inferences6, ds))
      case (state3, InferEvaluateSuccess(inferences6, _, deeplySatisfied)) => {
        // We have the mutability, but we can't know the entire kind just given a mutability.
        // Just hand upwards an unknown.
        (state3, InferEvaluateUnknown(inferences6, deeplySatisfied))
      }
    }
  }

  def makeMatcher(): InfererMatcher[Env, State] = {
    new InfererMatcher(
      templataTemplar,
      equator,
      evaluateRule,
      new IInfererMatcherDelegate[Env, State] {
        override def getAncestorInterfaceDistance(temputs0: State, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2) = {
          delegate.getAncestorInterfaceDistance(temputs0, descendantCitizenRef, ancestorInterfaceRef)
        }

        override def getMutability(state0: State, kind: Kind): Mutability = {
          delegate.getMutability(state0, kind)
        }

        override def lookupMemberTypes(state0: State, kind: Kind, expectedNumMembers: Int): Option[List[Coord]] = {
          delegate.lookupMemberTypes(state0, kind, expectedNumMembers)
        }

        override def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): (State, Boolean) = {
          delegate.citizenIsFromTemplate(state, citizen, template)
        }

        override def getAncestorInterfaces(temputs0: State, descendantCitizenRef: CitizenRef2): (State, Set[InterfaceRef2]) = {
          delegate.getAncestorInterfaces(temputs0, descendantCitizenRef)
        }
      })
  }

  private[infer] def getMemberCoords(
    state: State,
    inferences: Inferences,
    kind: Kind,
    // We hand this in because this is the number of pattern destructures they have.
    // This avoids a massive memory explosion if they hand us a million element array sequence.
    expectedNumMembers: Int):
  IInferEvaluateResult[List[Coord]] = {
    kind match {
      case sr @ StructRef2(_) => {
        val memberCoords = delegate.getMemberCoords(state, sr)
        if (memberCoords.size != expectedNumMembers) {
          return InferEvaluateConflict(inferences, "Expected something with " + expectedNumMembers + " members but received " + kind, List())
        }
        InferEvaluateSuccess(inferences, memberCoords, true)
      }
      case PackT2(members, _) => {
        if (members.size != expectedNumMembers) {
          return InferEvaluateConflict(inferences, "Expected something with " + expectedNumMembers + " members but received " + kind, List())
        }
        InferEvaluateSuccess(inferences, members, true)
      }
      case ArraySequenceT2(size, RawArrayT2(memberType, _)) => {
        // We need to do this check right here because right after this we're making an array of size `size`
        // which we just received as an integer from the user.
        if (size != expectedNumMembers) {
          return InferEvaluateConflict(inferences, "Expected something with " + expectedNumMembers + " members but received " + kind, List())
        }
        InferEvaluateSuccess(inferences, List(0 until size).map(_ => memberType), true)
      }
      case _ => {
        return InferEvaluateConflict(inferences, "Expected something destructurable but received " + kind, List())
      }
    }
  }
}
