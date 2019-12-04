package net.verdagon.radonc.templar.citizen

import net.verdagon.radonc.astronomer.ImplA
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{ExpressionLookupContext, IEnvironment, ImplEnvEntry, TemplataLookupContext}
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.{vassertSome, vfail, vwat}

import scala.collection.immutable.List

object ImplTemplar {

  private def getMaybeImplementedInterface(
    env: IEnvironment,
    temputs0: Temputs,
    childCitizenRef: CitizenRef2,
    impl1: ImplA):
  (Temputs, Option[InterfaceRef2]) = {
    val ImplA(codeLocation, rules, typeByRune, structKindRune, interfaceKindRune) = impl1

    val (temputs1, result) =
      InferTemplar.inferFromExplicitTemplateArgs(
        env,
        temputs0,
        List(structKindRune),
        rules,
        typeByRune,
        List(),
        None,
        List(KindTemplata(childCitizenRef)))

    result match {
      case isf @ InferSolveFailure(_, _, _, _, _, _) => {
        val (_) = isf
        (temputs1, None)
      }
      case InferSolveSuccess(inferences) => {
        inferences.templatasByRune(interfaceKindRune) match {
          case KindTemplata(interfaceKind @ InterfaceRef2(_)) => {
            (temputs1, Some(interfaceKind))
          }
          case it @ InterfaceTemplata(_, _) => {
            val (temputs2, interfaceRef) =
              StructTemplar.getInterfaceRef(temputs1, it, List())
            (temputs2, Some(interfaceRef))
          }
        }
      }
    }
  }

  def getParentInterfaces(
    temputs0: Temputs,
    childCitizenRef: CitizenRef2):
  (Temputs, List[InterfaceRef2]) = {
    val citizenEnv =
      childCitizenRef match {
        case sr @ StructRef2(_) => vassertSome(temputs0.envByStructRef.get(sr))
        case ir @ InterfaceRef2(_) => vassertSome(temputs0.envByInterfaceRef.get(ir))
      }
    citizenEnv.getAllTemplatasWithName(Templar.IMPL_NAME, Set(TemplataLookupContext, ExpressionLookupContext))
      .collect({
        case i @ ImplTemplata(_, _) => i
        case _ => vwat()
      })
      .foldLeft((temputs0, List[InterfaceRef2]()))({
        case ((temputs1, previousResults), ImplTemplata(implEnv, impl)) => {
          val (temputs2, newResults) =
            getMaybeImplementedInterface(implEnv, temputs1, childCitizenRef, impl);
          (temputs2, previousResults ++ newResults)
        }
      })
  }

  def getAncestorInterfaces(
    temputs0: Temputs,
    descendantCitizenRef: CitizenRef2):
  (Temputs, Set[InterfaceRef2]) = {
    val (temputs1, ancestorInterfacesWithDistance) =
      getAncestorInterfacesWithDistance(temputs0, descendantCitizenRef)
    (temputs1, ancestorInterfacesWithDistance.keySet)
  }

  def isAncestor(
    temputs0: Temputs,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (Temputs, Boolean) = {
    val (temputs1, ancestorInterfacesWithDistance) =
      getAncestorInterfacesWithDistance(temputs0, descendantCitizenRef)
    (temputs1, ancestorInterfacesWithDistance.contains(ancestorInterfaceRef))
  }

  def getAncestorInterfaceDistance(
    temputs0: Temputs,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (Temputs, Option[Int]) = {
    val (temputs1, ancestorInterfacesWithDistance) =
      getAncestorInterfacesWithDistance(temputs0, descendantCitizenRef)
    (temputs1, ancestorInterfacesWithDistance.get(ancestorInterfaceRef))
  }

  // Doesn't include self
  def getAncestorInterfacesWithDistance(
    temputs0: Temputs,
    descendantCitizenRef: CitizenRef2):
  (Temputs, Map[InterfaceRef2, Int]) = {
    val (temputs1, parentInterfaceRefs) =
      getParentInterfaces(temputs0, descendantCitizenRef)

    // Make a map that contains all the parent interfaces, with distance 1
    val foundSoFar = parentInterfaceRefs.map((_, 1)).toMap

    getAncestorInterfacesInner(
      temputs1,
      foundSoFar,
      1,
      parentInterfaceRefs.toSet)
  }

  private def getAncestorInterfacesInner(
    temputs0: Temputs,
    // This is so we can know what we've already searched.
    nearestDistanceByInterfaceRef: Map[InterfaceRef2, Int],
    // All the interfaces that are at most this distance away are inside foundSoFar.
    currentDistance: Int,
    // These are the interfaces that are *exactly* currentDistance away.
    // We will do our searching from here.
    interfacesAtCurrentDistance: Set[InterfaceRef2]):
  (Temputs, Map[InterfaceRef2, Int]) = {
    val (temputs10, interfacesAtNextDistance) =
      interfacesAtCurrentDistance.foldLeft((temputs0, Set[InterfaceRef2]()))({
        case ((temputs2, previousAncestorInterfaceRefs), parentInterfaceRef) => {
          val (temputs3, parentAncestorInterfaceRefs) =
            getParentInterfaces(temputs2, parentInterfaceRef)
          (temputs3, previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
        }
      })
    val nextDistance = currentDistance + 1

    // Discard the ones that have already been found; they're actually at
    // a closer distance.
    val newlyFoundInterfaces =
      interfacesAtNextDistance.diff(nearestDistanceByInterfaceRef.keySet)

    if (newlyFoundInterfaces.isEmpty) {
      (temputs10, nearestDistanceByInterfaceRef)
    } else {
      // Combine the previously found ones with the newly found ones.
      val newNearestDistanceByInterfaceRef =
        nearestDistanceByInterfaceRef ++
          newlyFoundInterfaces.map((_, nextDistance)).toMap

      getAncestorInterfacesInner(
        temputs10,
        newNearestDistanceByInterfaceRef,
        nextDistance,
        newlyFoundInterfaces)
    }
  }
}
