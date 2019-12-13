package net.verdagon.vale.carpenter

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar.CompleteProgram2
import net.verdagon.vale.templar.templata.Signature2
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}
import net.verdagon.vale.vassert

import scala.collection.immutable.Set

object Carpenter {
  def translate(program2: CompleteProgram2): Hinputs = {
    val (edgeBlueprints, edges) = EdgeCarpenter.assembleEdges(program2.functions, program2.interfaces, program2.impls)

    // NEVER ZIP TWO SETS TOGETHER
    val edgeBlueprintsAsList = edgeBlueprints.toList
    val edgeBlueprintsByInterface = edgeBlueprintsAsList.map(_.interface).zip(edgeBlueprintsAsList).toMap;

    edgeBlueprintsByInterface.foreach({ case (interfaceRef, edgeBlueprint) =>
      vassert(edgeBlueprint.interface == interfaceRef)
    })


    val functionIdsBySignature: Map[Signature2, Int] =
      program2.functions.map(_.header.toSignature).zipWithIndex.toMap

    val interfaceRefs: Set[InterfaceRef2] = program2.interfaces.map(_.getRef).toSet
    val interfaceIdsByInterface: Map[InterfaceRef2, Int] = interfaceRefs.zipWithIndex.toMap
    val structIdStart = interfaceIdsByInterface.size

    val edgeBlueprintsByInterfaceId =
      edgeBlueprints.map(_.interface).map(interfaceIdsByInterface).zip(edgeBlueprints).toMap

    val structRefs: Set[StructRef2] = program2.structs.map(_.getRef).toSet
    val structIds = structIdStart until (structIdStart + structRefs.size)
    val structIdsByStruct: Map[StructRef2, Int] = structRefs.zip(structIds).toMap

    val etablesByStructRef = ETableGenerator.generateETables(interfaceIdsByInterface, edges)

    Hinputs(
      program2,
      edgeBlueprintsByInterface,
      edgeBlueprintsByInterfaceId,
      edges,
      functionIdsBySignature,
      structIdsByStruct,
      interfaceIdsByInterface,
      etablesByStructRef)
  }
}
