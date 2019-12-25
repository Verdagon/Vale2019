package net.verdagon.vale.carpenter

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar.CompleteProgram2
import net.verdagon.vale.vassert

object Carpenter {
  def translate(program2: CompleteProgram2): Hinputs = {
    val (edgeBlueprints, edges) = EdgeCarpenter.assembleEdges(program2.functions, program2.interfaces, program2.impls)

    // NEVER ZIP TWO SETS TOGETHER
    val edgeBlueprintsAsList = edgeBlueprints.toList
    val edgeBlueprintsByInterface = edgeBlueprintsAsList.map(_.interface).zip(edgeBlueprintsAsList).toMap;

    edgeBlueprintsByInterface.foreach({ case (interfaceRef, edgeBlueprint) =>
      vassert(edgeBlueprint.interface == interfaceRef)
    })

    Hinputs(
      program2,
      edgeBlueprintsByInterface,
      edges)
  }
}
