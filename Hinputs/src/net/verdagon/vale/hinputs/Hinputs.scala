package net.verdagon.vale.hinputs

import net.verdagon.vale.templar.{CompleteProgram2, Edge2, InterfaceEdgeBlueprint, Program2}
import net.verdagon.vale.templar.templata.{FunctionBanner2, Prototype2, Signature2}
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}

case class ETable2(struct: StructRef2, table: TetrisTable[InterfaceRef2, InterfaceRef2])

case class Hinputs(
  program2: CompleteProgram2,
  edgeBlueprintsByInterface: Map[InterfaceRef2, InterfaceEdgeBlueprint],
  edges: Set[Edge2])
