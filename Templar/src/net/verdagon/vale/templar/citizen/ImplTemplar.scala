package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.ImplA
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.{vassertSome, vfail, vwat}

import scala.collection.immutable.List

object ImplTemplar {

  private def getMaybeImplementedInterface(
    env: IEnvironment,
    temputs: TemputsBox,
    childCitizenRef: CitizenRef2,
    impl1: ImplA):
  (Option[InterfaceRef2]) = {
    val ImplA(codeLocation, rules, typeByRune, structKindRune, interfaceKindRune) = impl1

    val result =
      InferTemplar.inferFromExplicitTemplateArgs(
        env,
        temputs,
        List(structKindRune),
        rules,
        typeByRune,
        List(),
        None,
        List(KindTemplata(childCitizenRef)))

    result match {
      case isf @ InferSolveFailure(_, _, _, _, _, _) => {
        val _ = isf
        (None)
      }
      case InferSolveSuccess(inferences) => {
        inferences.templatasByRune(interfaceKindRune) match {
          case KindTemplata(interfaceKind @ InterfaceRef2(_)) => {
            (Some(interfaceKind))
          }
          case it @ InterfaceTemplata(_, _) => {
            val interfaceRef =
              StructTemplar.getInterfaceRef(temputs, it, List())
            (Some(interfaceRef))
          }
        }
      }
    }
  }

  def getParentInterfaces(
    temputs: TemputsBox,
    childCitizenRef: CitizenRef2):
  (List[InterfaceRef2]) = {
    val citizenEnv =
      childCitizenRef match {
        case sr @ StructRef2(_) => vassertSome(temputs.envByStructRef.get(sr))
        case ir @ InterfaceRef2(_) => vassertSome(temputs.envByInterfaceRef.get(ir))
      }
    citizenEnv.getAllTemplatasWithName(Templar.IMPL_NAME, Set(TemplataLookupContext, ExpressionLookupContext))
      .collect({
        case i @ ImplTemplata(_, _) => i
        case _ => vwat()
      })
      .foldLeft((List[InterfaceRef2]()))({
        case ((previousResults), ImplTemplata(implEnv, impl)) => {
          val newResults =
            getMaybeImplementedInterface(implEnv, temputs, childCitizenRef, impl);
          (previousResults ++ newResults)
        }
      })
  }

  def getAncestorInterfaces(
    temputs: TemputsBox,
    descendantCitizenRef: CitizenRef2):
  (Set[InterfaceRef2]) = {
    val ancestorInterfacesWithDistance =
      getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
    (ancestorInterfacesWithDistance.keySet)
  }

  def isAncestor(
    temputs: TemputsBox,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (Boolean) = {
    val ancestorInterfacesWithDistance =
      getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
    (ancestorInterfacesWithDistance.contains(ancestorInterfaceRef))
  }

  def getAncestorInterfaceDistance(
    temputs: TemputsBox,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (Option[Int]) = {
    val ancestorInterfacesWithDistance =
      getAncestorInterfacesWithDistance(temputs, descendantCitizenRef)
    (ancestorInterfacesWithDistance.get(ancestorInterfaceRef))
  }

  // Doesn't include self
  def getAncestorInterfacesWithDistance(
    temputs: TemputsBox,
    descendantCitizenRef: CitizenRef2):
  (Map[InterfaceRef2, Int]) = {
    val parentInterfaceRefs =
      getParentInterfaces(temputs, descendantCitizenRef)

    // Make a map that contains all the parent interfaces, with distance 1
    val foundSoFar = parentInterfaceRefs.map((_, 1)).toMap

    getAncestorInterfacesInner(
      temputs,
      foundSoFar,
      1,
      parentInterfaceRefs.toSet)
  }

  private def getAncestorInterfacesInner(
    temputs: TemputsBox,
    // This is so we can know what we've already searched.
    nearestDistanceByInterfaceRef: Map[InterfaceRef2, Int],
    // All the interfaces that are at most this distance away are inside foundSoFar.
    currentDistance: Int,
    // These are the interfaces that are *exactly* currentDistance away.
    // We will do our searching from here.
    interfacesAtCurrentDistance: Set[InterfaceRef2]):
  (Map[InterfaceRef2, Int]) = {
    val interfacesAtNextDistance =
      interfacesAtCurrentDistance.foldLeft((Set[InterfaceRef2]()))({
        case ((previousAncestorInterfaceRefs), parentInterfaceRef) => {
          val parentAncestorInterfaceRefs =
            getParentInterfaces(temputs, parentInterfaceRef)
          (previousAncestorInterfaceRefs ++ parentAncestorInterfaceRefs)
        }
      })
    val nextDistance = currentDistance + 1

    // Discard the ones that have already been found; they're actually at
    // a closer distance.
    val newlyFoundInterfaces =
      interfacesAtNextDistance.diff(nearestDistanceByInterfaceRef.keySet)

    if (newlyFoundInterfaces.isEmpty) {
      (nearestDistanceByInterfaceRef)
    } else {
      // Combine the previously found ones with the newly found ones.
      val newNearestDistanceByInterfaceRef =
        nearestDistanceByInterfaceRef ++
          newlyFoundInterfaces.map((_, nextDistance)).toMap

      getAncestorInterfacesInner(
        temputs,
        newNearestDistanceByInterfaceRef,
        nextDistance,
        newlyFoundInterfaces)
    }
  }
}
