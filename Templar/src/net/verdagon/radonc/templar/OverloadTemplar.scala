package net.verdagon.radonc.templar

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.radonc.scout.rules.{EqualsSR, TemplexSR, TypedSR}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata.{IPotentialBanner, _}
import net.verdagon.radonc.scout.{CodeBody1, ITemplexS}
import net.verdagon.radonc.templar.env.{ExpressionLookupContext, FunctionEnvironment, IEnvironment, TemplataLookupContext}
import net.verdagon.radonc.templar.function.FunctionTemplar
import net.verdagon.radonc.templar.function.FunctionTemplar.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.templar.infer.inferer.InfererEvaluator
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object OverloadTemplar {
  def scoutMaybeFunctionForPrototype(
      // The environment to look in.
      env: IEnvironment,
      temputs0: Temputs,
      humanName: String,
      explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
      args: List[ParamFilter],
      exact: Boolean):
  (
    Temputs,
    Option[Prototype2],
    // All the ones that could have worked, but were outscored by the best match
    Map[IPotentialBanner, String],
    // All the banners we rejected, and the reason why
    Map[FunctionBanner2, String],
    // All the FunctionA we rejected, and the reason why
    Map[FunctionA, String]
  ) = {
    val (temputs1, maybePotentialBanner, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction) =
      scoutPotentialFunction(
        env, temputs0, humanName, explicitlySpecifiedTemplateArgTemplexesS, args, exact)
    maybePotentialBanner match {
      case None => {
        (temputs1, None, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction)
      }
      case Some(potentialBanner) => {
        val (temputs2, thing) =
          stampPotentialFunctionForPrototype(
            env, temputs1, potentialBanner, args)
        (temputs2, Some(thing), outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction)
      }
    }
  }

  sealed trait IScoutExpectedFunctionResult
  case class ScoutExpectedFunctionSuccess(prototype: Prototype2) extends IScoutExpectedFunctionResult
  case class ScoutExpectedFunctionFailure(
    humanName: String,
    args: List[ParamFilter],
    // All the ones that could have worked, but were outscored by the best match
    outscoredReasonByPotentialBanner: Map[IPotentialBanner, String],
    // All the banners we rejected, and the reason why
    rejectedReasonByBanner: Map[FunctionBanner2, String],
    // All the FunctionA we rejected, and the reason why
    rejectedReasonByFunction: Map[FunctionA, String]
  ) extends IScoutExpectedFunctionResult {
    override def toString = {
      "Couldn't find a " + humanName + "(" + args.map(":" + _).mkString(", ") + ")\n" +
        "Outscored:\n" + outscoredReasonByPotentialBanner.map({
        case (potentialBanner, outscoredReason) => potentialBanner + ": " + outscoredReason
      }).mkString("\n") + "\n" +
        "Rejected:\n" + rejectedReasonByBanner.map({
        case (banner, rejectedReason) => banner + ": " + rejectedReason
      }).mkString("\n") + "\n" +
        "Rejected:\n" + rejectedReasonByFunction.map({
        case (functionS, rejectedReason) => functionS + ": " + rejectedReason
      }).mkString("\n") + "\n"
    }
  }

  def scoutExpectedFunctionForPrototype(
    env: IEnvironment,
    temputs0: Temputs,
    humanName: String,
    explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
    args: List[ParamFilter],
    exact: Boolean):
  (Temputs, IScoutExpectedFunctionResult) = {
    val (temputs1, maybeFunction, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction) =
      scoutMaybeFunctionForPrototype(
        env, temputs0, humanName, explicitlySpecifiedTemplateArgTemplexesS, args, exact)
    maybeFunction match {
      case None => {
        (temputs1, ScoutExpectedFunctionFailure(humanName, args, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction))
      }
      case Some(function) => {
        (temputs1, ScoutExpectedFunctionSuccess(function))
      }
    }
  }

  private def paramMatches(
    temputs0: Temputs,
    source: Coord,
    destination: Coord,
    exact: Boolean):
  (
    Temputs,
    // Rejection reason, if any. None means it matches.
    Option[String]
  ) = {
    if (exact) {
      if (source == destination) {
        (temputs0, None)
      } else {
        (temputs0, Some(source + " is not " + destination))
      }
    } else {
      TemplataTemplar.isTypeConvertible(temputs0, source, destination) match {
        case (temputs1, true) => (temputs1, None)
        case (temputs1, false) => (temputs1, Some(source + " cannot convert to " + destination))
      }
    }
  }

  private def paramsMatch(
    temputs0: Temputs,
    desiredParams: List[ParamFilter],
    candidateParams: List[Parameter2],
    exact: Boolean):
  (
    Temputs,
    // Rejection reason, if any. None means it matches.
    Option[String]
  ) = {
    if (desiredParams.size != candidateParams.size) {
      return (temputs0, Some("Number of params doesn't match! Supplied " + desiredParams.size + " but function takes " + candidateParams.size))
    }
    val temputs10 =
      desiredParams.zip(candidateParams).zipWithIndex.foldLeft(temputs0)({
        case (temputs1, ((desiredParam, candidateParam), paramIndex)) => {
          val ParamFilter(desiredTemplata, desiredMaybeVirtuality) = desiredParam
          val Parameter2(_, candidateMaybeVirtuality, candidateType) = candidateParam
          val temputs3 =
            paramMatches(temputs1, desiredTemplata, candidateType, exact) match {
              case (temputs2, Some(rejectionReason)) => return (temputs2, Some("Param at index " + paramIndex + " doesn't match: " + rejectionReason))
              case (temputs2, None) => temputs2
            }
          ((desiredMaybeVirtuality, candidateMaybeVirtuality) match {
            case (None, _) =>
            case (desiredVirtuality, candidateVirtuality) => {
              if (desiredVirtuality != candidateVirtuality) {
                return (temputs3, Some("Virtualities don't match at index " + paramIndex))
              }
            }
          })
          temputs3
        }
      })
    // Would have bailed out early if there was a false
    (temputs10, None)
  }

  private def getCandidateBanners(
    env: IEnvironment,
    temputs0: Temputs,
    humanName: String,
    explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
    paramFilters: List[ParamFilter],
    exact: Boolean):
  (
    Temputs,
    Set[IPotentialBanner],
    // rejection reason by banner
    Map[FunctionBanner2, String],
    // rejection reason by function
    Map[FunctionA, String]
  ) = {
    val hayTemplatas = findHayTemplatas(env, temputs0, humanName, paramFilters)

    val (temputs10, allPotentialBanners, allRejectionReasonByBanner, allRejectionReasonByFunction) =
      hayTemplatas.foldLeft((temputs0, Set[IPotentialBanner](), Map[FunctionBanner2, String](), Map[FunctionA, String]()))({
        case ((temputs1, previousPotentials, previousRejectionReasonByBanner, previousRejectionReasonByFunction), templata) => {
          val (temputs8, potentialBanners, rejectionReasonByBanner, rejectionReasonByFunction) =
            templata match {
              case KindTemplata(OverloadSet(overloadsEnv, nameInOverloadsEnv, _)) => {
                getCandidateBanners(
                  overloadsEnv, temputs1, nameInOverloadsEnv, explicitlySpecifiedTemplateArgTemplexesS, paramFilters, exact)
              }
              case KindTemplata(sr @ StructRef2(_)) => {
                val structEnv = temputs1.envByStructRef(sr)
                getCandidateBanners(
                  structEnv, temputs1, CallTemplar.CALL_FUNCTION_NAME, explicitlySpecifiedTemplateArgTemplexesS, paramFilters, exact)
              }
              case KindTemplata(sr @ InterfaceRef2(_)) => {
                val interfaceEnv = temputs1.envByInterfaceRef(sr)
                getCandidateBanners(
                  interfaceEnv, temputs1, CallTemplar.CALL_FUNCTION_NAME, explicitlySpecifiedTemplateArgTemplexesS, paramFilters, exact)
              }
              case ExternFunctionTemplata(header) => {
                paramsMatch(temputs1, paramFilters, header.params, exact) match {
                  case (temputs2, None) => {
                    (temputs2, List(PotentialBannerFromExternFunction(header)), Map(), Map())
                  }
                  case (temputs2, Some(rejectionReason)) => {
                    (temputs2, List(), Map(header.toBanner -> rejectionReason), Map())
                  }
                }
              }
              case ft @ FunctionTemplata(_, functionA) => {
                functionA.tyype match {
                  case TemplateTemplataType(identifyingRuneTemplataTypes, FunctionTemplataType) => {

                    val ruleTyper =
                      new RuleTyperEvaluator[IEnvironment, Temputs](
                        new IRuleTyperEvaluatorDelegate[IEnvironment, Temputs] {
                          override def lookupType(state: Temputs, env: IEnvironment, name: String): (Temputs, ITemplataType) = {
                            val templata =
                              env.getNearestTemplataWithName(name, Set(TemplataLookupContext)) match {
                                case None => vfail("Nothing found with name " + name)
                                case Some(t) => t
                              }
                            (state, templata.tyype)
                          }
                        }
                      )

                    if (explicitlySpecifiedTemplateArgTemplexesS.size > identifyingRuneTemplataTypes.size) {
                      vfail("Supplied more arguments than there are identifying runes!")
                    }

                    // Now that we know what types are expected, we can FINALLY rule-type these explicitly
                    // specified template args! (The rest of the rule-typing happened back in the astronomer,
                    // this is the one time we delay it, see MDRTCUT).

                    // There might be less explicitly specified template args than there are types, and that's
                    // fine. Hopefully the rest will be figured out by the rule evaluator.
                    val runeNames = explicitlySpecifiedTemplateArgTemplexesS.indices.toList.map("Rune" + _)
                    val equalsRules =
                      explicitlySpecifiedTemplateArgTemplexesS.zip(identifyingRuneTemplataTypes).zip(runeNames).map({
                        case ((explicitlySpecifiedTemplateArgTemplexS, identifyingRuneTemplataType), runeName) => {
                          EqualsSR(
                            TypedSR(Some(runeName), Conversions.unevaluateTemplataType(identifyingRuneTemplataType)),
                            TemplexSR(explicitlySpecifiedTemplateArgTemplexS))
                        }
                      })

                    ruleTyper.solve(temputs1, env, equalsRules, List(), Some(runeNames.toSet)) match {
                      case (temputs2, rtsf @ RuleTyperSolveFailure(_, _, _)) => (temputs2, List(), Map(), Map(functionA -> ("Specified template args don't match expected types!\nExpected types: (" + identifyingRuneTemplataTypes.mkString(",") + ")\nSpecified template args: " + explicitlySpecifiedTemplateArgTemplexesS + "\nCause: " + rtsf.toString)))
                      case (temputs2, RuleTyperSolveSuccess(runeTypeConclusions, rulesA)) => {
                        InferTemplar.inferFromExplicitTemplateArgs(env, temputs2, List(), rulesA, runeTypeConclusions.typeByRune, List(), None, List()) match {
                          case (temputs3, isf @ InferSolveFailure(_, _, _, _, _, _)) => (temputs3, List(), Map(), Map(functionA -> ("Couldn't evaluate template args: " + isf.toString)))
                          case (temputs3, InferSolveSuccess(inferences)) => {
                            val explicitlySpecifiedTemplateArgTemplatas = runeNames.map(inferences.templatasByRune)

                            FunctionTemplar.evaluateTemplatedFunctionFromCallForBanner(
                              temputs3, ft, explicitlySpecifiedTemplateArgTemplatas, paramFilters) match {
                              case (temputs4, EvaluateFunctionFailure(reason)) => {
                                (temputs4, List(), Map(), Map(functionA -> reason))
                              }
                              case (temputs4, EvaluateFunctionSuccess(banner)) => {
                                paramsMatch(temputs4, paramFilters, banner.params, exact) match {
                                  case (temputs7, Some(rejectionReason)) => {
                                    (temputs7, List(), Map(banner -> rejectionReason), Map())
                                  }
                                  case (temputs7, None) => {
                                    (temputs7, List(PotentialBannerFromFunctionS(banner, ft)), Map(), Map())
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                  case FunctionTemplataType => {
                    val (temputs2, banner) =
                      FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForBanner(
                        temputs1, ft)
                    paramsMatch(temputs2, paramFilters, banner.params, exact) match {
                      case (temputs7, None) => {
                        (temputs7, List(PotentialBannerFromFunctionS(banner, ft)), Map(), Map())
                      }
                      case (temputs7, Some(rejectionReason)) => {
                        (temputs7, List(), Map(banner -> rejectionReason), Map())
                      }
                    }
                  }
                }
              }
            }
          (temputs8, previousPotentials ++ potentialBanners, previousRejectionReasonByBanner ++ rejectionReasonByBanner, previousRejectionReasonByFunction ++ rejectionReasonByFunction)
        }
      })
    (temputs10, allPotentialBanners, allRejectionReasonByBanner, allRejectionReasonByFunction)
  }

  // Gets all the environments for all the arguments.
  private def getParamEnvironments(temputs: Temputs, paramFilters: List[ParamFilter]):
  List[IEnvironment] = {
    paramFilters.flatMap({ case ParamFilter(tyype, virtuality) =>
      (tyype.referend match {
        case sr @ StructRef2(_) => List(temputs.envByStructRef(sr))
        case ir @ InterfaceRef2(_) => List(temputs.envByInterfaceRef(ir))
        case _ => List()
      }) ++
        (virtuality match {
          case None => List()
          case Some(Abstract2) => List()
          case Some(Override2(ir)) => List(temputs.envByInterfaceRef(ir))
        })
    })
  }

  // Looks in all the environments of the given arguments for something with the given name.
  private def findHayTemplatas(
      env: IEnvironment,
      temputs: Temputs,
      humanName: String,
      paramFilters: List[ParamFilter]):
  Set[ITemplata] = {
    val environments = env :: getParamEnvironments(temputs, paramFilters)
    environments.flatMap(_.getAllTemplatasWithName(humanName, Set(ExpressionLookupContext))).toSet
  }

  // Checks to see if there's a function that *could*
  // exist that takes in these parameter types, and returns what the signature *would* look like.
  // Only considers when arguments match exactly.
  // If given something in maybeSuperInterfaceRef2, it will search for a function that
  // overrides that interfaceRef2 in that position. If we ever support multimethods we
  // might need to take a list of these, same length as the arg types... or combine
  // them somehow.
  def scoutPotentialFunction(
      env: IEnvironment,
      temputs0: Temputs,
      humanName: String,
      explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
      args: List[ParamFilter],
      exact: Boolean):
  (
    Temputs,
    // Best match, if any
    Option[IPotentialBanner],
    // All the ones that could have worked, but were outscored by the best match
    Map[IPotentialBanner, String],
    // All the banners we rejected, and the reason why
    Map[FunctionBanner2, String],
    // All the FunctionA we rejected, and the reason why
    Map[FunctionA, String]
  ) = {
    val (temputs3, candidateBanners, rejectionReasonByBanner, rejectionReasonByFunction) =
      getCandidateBanners(env, temputs0, humanName, explicitlySpecifiedTemplateArgTemplexesS, args, exact);
    if (candidateBanners.isEmpty) {
      (temputs3, None, Map(), rejectionReasonByBanner, rejectionReasonByFunction)
    } else if (candidateBanners.size == 1) {
      (temputs3, Some(candidateBanners.head), Map(), rejectionReasonByBanner, rejectionReasonByFunction)
    } else {
      val (temputs5, best, outscoreReasonByBanner) =
        narrowDownCallableOverloads(temputs0, candidateBanners, args.map(_.tyype))
      (temputs5, Some(best), outscoreReasonByBanner, rejectionReasonByBanner, rejectionReasonByFunction)
    }
  }

  private def getBannerParamScores(
    temputs0: Temputs,
    banner: IPotentialBanner,
    argTypes: List[Coord]):
  (Temputs, List[TypeDistance]) = {
    banner.banner.paramTypes.zip(argTypes)
      .foldLeft((temputs0, List[TypeDistance]()))({
        case ((temputs1, previousParamsScores), (paramType, argType)) => {
          TemplataTemplar.getTypeDistance(temputs1, argType, paramType) match {
            case (_, None) => vfail("wat")
            case (temputs2, Some(distance)) => (temputs2, previousParamsScores :+ distance)
          }
        }
      })
  }

  private def narrowDownCallableOverloads(
      temputs0: Temputs,
      unfilteredBanners: Set[IPotentialBanner],
      argTypes: List[Coord]):
  (
    Temputs,
    IPotentialBanner,
    // Rejection reason by banner
    Map[IPotentialBanner, String]) = {

    // Sometimes a banner might come from many different environments (remember,
    // when we do a call, we look in the environments of all the arguments' types).
    // Here we weed out these duplicates.
    val dedupedBanners =
      unfilteredBanners.foldLeft(List[IPotentialBanner]())({
        case (potentialBannerByBannerSoFar, currentPotentialBanner) => {
          if (potentialBannerByBannerSoFar.exists(_.banner == currentPotentialBanner.banner)) {
            potentialBannerByBannerSoFar
          } else {
            potentialBannerByBannerSoFar :+ currentPotentialBanner
          }
        }
      })

    // If there are multiple overloads with the same exact parameter list,
    // then get rid of the templated ones; ordinary ones get priority.
    val banners =
      dedupedBanners.groupBy(_.banner.paramTypes).values.flatMap({ potentialBannersWithSameParamTypes =>
        val ordinaryBanners =
          potentialBannersWithSameParamTypes.filter({
            case PotentialBannerFromFunctionS(_, function) => !function.function.isTemplate
            case PotentialBannerFromExternFunction(_) => true
          })
        if (ordinaryBanners.isEmpty) {
          // No ordinary banners, so include all the templated ones
          potentialBannersWithSameParamTypes
        } else {
          // There are some ordinary banners, so only consider the ordinary banners
          ordinaryBanners
        }
      }).toList

    val (temputs10, bannersAndScores) =
      banners.foldLeft((temputs0, List[(IPotentialBanner, List[TypeDistance])]()))({
        case ((temputs1, previousBannersAndScores), banner) => {
          val (temputs2, scores) =
            getBannerParamScores(temputs1, banner, argTypes)
          (temputs2, previousBannersAndScores :+ (banner, scores))
        }
      })

    val bestScore =
      bannersAndScores.map(_._2).reduce((aScore, bScore) => {
        if (aScore == bScore) {
          // Doesn't matter, just return one
          aScore
        } else {
          val aIsBetter =
            aScore.zip(bScore).forall({
              case (aScorePart, bScorePart) => aScorePart.lessThanOrEqualTo(bScorePart)
            })
          if (aIsBetter) aScore else bScore
        }
      })

    val bannerByIsBestScore =
      bannersAndScores.groupBy[Boolean]({ case (_, score) => score == bestScore })


    val bannerWithBestScore =
      if (bannerByIsBestScore.getOrElse(true, List()).isEmpty) {
        vfail("wat")
      } else if (bannerByIsBestScore.getOrElse(true, List()).size > 1) {
        vfail("Can't resolve between:\n" + bannerByIsBestScore.mkString("\n"))
      } else {
        bannerByIsBestScore(true).head._1
      };

    val rejectedBanners =
      bannerByIsBestScore.getOrElse(false, List()).map(_._1)
    val rejectionReasonByBanner =
      rejectedBanners.map((_, "TODO: rejection reason here")).toMap

    (temputs10, bannerWithBestScore, rejectionReasonByBanner)
  }

  def stampPotentialFunctionForBanner(
      env: IEnvironment,
      temputs0: Temputs,
      potentialBanner: IPotentialBanner):
  (Temputs, FunctionBanner2) = {
    potentialBanner match {
      case PotentialBannerFromFunctionS(signature, functionTemplata @ FunctionTemplata(_, functionS)) => {
        if (functionS.isTemplate) {
          val (temputs1, EvaluateFunctionSuccess(banner)) =
            FunctionTemplar.evaluateTemplatedLightFunctionFromCallForBanner(
              temputs0, functionTemplata, List(), signature.paramTypes.map(p => ParamFilter(p, None)));
          (temputs1, banner)
        } else {
          FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForBanner(
            temputs0, functionTemplata)
        }
      }
      case PotentialBannerFromExternFunction(header) => {
        (temputs0, header.toBanner)
      }
    }
  }

  // The "for temputs" thing is important, it means we don't care what the result is, we just
  // want to make sure it gets into the outputs.
  private def stampPotentialFunctionForPrototype(
      env: IEnvironment,
      temputs0: Temputs,
      potentialBanner: IPotentialBanner,
      args: List[ParamFilter]):
  (Temputs, Prototype2) = {
    potentialBanner match {
      case PotentialBannerFromFunctionS(signature, ft @ FunctionTemplata(_, functionS)) => {
        if (functionS.isTemplate) {
          FunctionTemplar.evaluateTemplatedFunctionFromCallForPrototype(
              temputs0, ft, signature.fullName.steps.last.templateArgs.get, args) match {
            case (temputs2, EvaluateFunctionSuccess(prototype)) => (temputs2, prototype)
            case (_, eff @ EvaluateFunctionFailure(_)) => vfail(eff.toString)
          }
        } else {
          // debt: look into making FunctionTemplar's methods accept function templatas
          // so we dont pass in the wrong environment again
          FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForPrototype(
            temputs0, ft)
        }
      }
      case PotentialBannerFromExternFunction(header) => {
        (temputs0, header.toPrototype)
      }
    }
//    temputs0.functions.find(_.header.toBanner == banner) match {
//      case Some(existingFunction) => {
//        // Then it was already stamped/evaluated. This is the case if it came from
//        // a light lambda. We have to do this because the env.functions1ByOrdinarySignature
//        // will fail down there, because lambdas aren't included there...
//        (temputs0, existingFunction.header.toPrototype)
//      }
//      case None => {
//        // Then the best banner came from an ordinary banner. Let's speed up its evaluating to now.
//        val maybeOriginFunction = banner.originFunction;
//        maybeOriginFunction match {
//          case None => {
//            vfail("?") // what do we do when we want to stamp something with no origin function?
//          }
//          case Some(originFunction) => {
//            originFunction.body match {
//              case CodeBody1(block) => {
//                FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForPrototype(
//                  env, temputs0, originFunction)
//              }
//              case _ => {
//                vfail("?") // what do we do when we want to stamp an abstract function?
//                // val (temputs1, header) =
//                //   FunctionTemplar.evaluateOrdinaryLightAbstractFunctionForHeader(env, temputs0, originFunction)
//                // (temputs1, header.toPrototype)
//              }
//            }
//          }
//        }
//      }
  }
}
