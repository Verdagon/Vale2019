package net.verdagon.radonc.templar.function

import net.verdagon.radonc.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env.IEnvironment
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object VirtualTemplar {
  // See Virtuals doc for this function's purpose.
  def evaluateOverrides(
      env: IEnvironment, temputs0: Temputs, sparkHeader: FunctionHeader2):
  Temputs = {
//    vassert(sparkHeader.params.count(_.virtuality.nonEmpty) <= 1)
//    val maybeInterfaceRefAndIndex =
//      sparkHeader.params.zipWithIndex.collectFirst({
//        case (Parameter2(_, Some(Abstract2), Coord(_, ir @ InterfaceRef2(_))), index) => (ir, index)
//      })
//
//    maybeInterfaceRefAndIndex match {
//      case None => {
//        // It's not abstract, so nothing to do here.
        temputs0
//      }
//      case Some((interfaceRef2, index)) => {
//        val interfaceDef2 = temputs0.lookupInterface(interfaceRef2)
//        val descendantCitizens =
//          StructTemplar.getDescendants(interfaceDef2, false)
//        evaluateDescendantsOverrideBanners(
//          env, temputs0, sparkHeader, descendantCitizens, index)
//      }
//    }
  }
//
//  private def evaluateDescendantsOverrideBanners(
//    env: IEnvironment,
//    temputs0: Temputs,
//    header: FunctionHeader2,
//    descendants: Set[CitizenRef2],
//    virtualParamIndex: Int):
//  Temputs = {
//    descendants
//      .foldLeft(temputs0)({
//        case (temputs1, descendantCitizen) => {
//          val oldParams = header.params.map(_.tyype)
//          val paramFiltersWithoutOverride = oldParams.map(oldParam => ParamFilter(oldParam, None))
//
//          val Coord(ownership, interfaceRef2 @ InterfaceRef2(_)) = oldParams(virtualParamIndex)
//
//          val newParamFilter = ParamFilter(Coord(ownership, descendantCitizen), Some(Override2(interfaceRef2)))
//          val paramFilters = paramFiltersWithoutOverride.updated(virtualParamIndex, newParamFilter)
//
//          val (temputs2, maybeOverridePotentialBanner, _, _, _) =
//            OverloadTemplar.scoutPotentialFunction(
//              env,
//              temputs1,
//              header.humanName,
//              List(),
//              paramFilters,
//              exact = true);
//          maybeOverridePotentialBanner match {
//            case None => {
//              // What happens here? do we use a default implementation or something?
//              vfail("what")
//            }
//            case Some(overridePotentialBanner) => {
//              val (temputs3, _) =
//                OverloadTemplar.stampPotentialFunctionForBanner(
//                  env, temputs2, overridePotentialBanner)
//              temputs3
//            }
//          }
//        }
//      });
//  }

  // For the "Templated parent case"
  def evaluateParent(
    env: IEnvironment, temputs0: Temputs, sparkHeader: FunctionHeader2):
  Temputs = {
    vassert(sparkHeader.params.count(_.virtuality.nonEmpty) <= 1)
    val maybeSuperInterfaceAndIndex =
      sparkHeader.params.zipWithIndex.collectFirst({
        case (Parameter2(_, Some(Override2(ir)), Coord(_, StructRef2(_))), index) => (ir, index)
      })

    maybeSuperInterfaceAndIndex match {
      case None => {
        // It's not an override, so nothing to do here.
        temputs0
      }
      case Some((superInterfaceRef2, virtualIndex)) => {
        val overrideFunctionParamTypes = sparkHeader.params.map(_.tyype)
        val needleSuperFunctionParamTypes =
          overrideFunctionParamTypes.zipWithIndex.map({ case (paramType, index) =>
            if (index != virtualIndex) {
              paramType
            } else {
              paramType.copy(referend = superInterfaceRef2)
            }
          })

        val needleSuperFunctionParamFilters =
          needleSuperFunctionParamTypes.zipWithIndex.map({
            case (needleSuperFunctionParamType, index) => {
              ParamFilter(needleSuperFunctionParamType, if (index == virtualIndex) Some(Abstract2) else None)
            }
          })

        OverloadTemplar.scoutExpectedFunctionForPrototype(
          env, temputs0, sparkHeader.fullName.steps.last.humanName, List(), needleSuperFunctionParamFilters, true) match {
          case (temputs2, ScoutExpectedFunctionSuccess(_)) => {
            // Throw away the prototype, we just want it to be in the temputs.
            temputs2
          }
          case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => vfail(seff.toString)
        }
      }
    }
  }
}
