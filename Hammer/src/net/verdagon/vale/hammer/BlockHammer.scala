package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar.Block2
import net.verdagon.vale.templar.types.Raw
import net.verdagon.vale.{vassert, vfail}

object BlockHammer {

  def newId(nodesByLine: Vector[NodeH]) = Hammer.newId(nodesByLine)

  def addLine(nodesByLine: Vector[NodeH], node: NodeH): (Vector[NodeH], NodeH) = {
    (nodesByLine :+ node, node)
  }

  def translateBlock(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    newStackHeight: StackHeight,
    block2: Block2):
  (Hamuts, Locals, BlockH, Option[RegisterAccessH[ReferendH]]) = {

    // This means the caller didn't make us a new stack height.
    vassert(newStackHeight.localsHeight == newStackHeight.blockStartLocalsHeight)

    val nodesByLine0 = Vector[NodeH]();

    val (hamuts6, locals1, stackHeight1, nodesByLine4, registerAccesses, deferreds) =
      ExpressionHammer.translateMaybeReturningExpressions(
        hinputs, hamuts0, locals0, newStackHeight, nodesByLine0, block2.exprs);

    vassert(deferreds.isEmpty) // curious, do we have to do any here

    val localIdsInThisBlock = locals1.locals.keys.toSet.diff(locals0.locals.keys.toSet)
    vassert(stackHeight1.localsHeight - stackHeight1.blockStartLocalsHeight == localIdsInThisBlock.size)
    val localsInThisBlock = localIdsInThisBlock.map(locals1.locals)
    vassert(localsInThisBlock.map(_.height).size == localsInThisBlock.size)
    val unstackifiedLocalIdsInThisBlock = locals1.unstackifiedVars.intersect(localIdsInThisBlock)

//    if (localIdsInThisBlock != unstackifiedLocalIdsInThisBlock) {
//      // This probably means that there was no UnletH or DestructureH for that variable.
//      vfail("Ununstackified local: " + (localIdsInThisBlock -- unstackifiedLocalIdsInThisBlock))
//    }

    val resultType = registerAccesses.last.map(_.expectedType).getOrElse(ReferenceH(Raw, VoidH()))
//    start here, we're returning locals0 and thats not optimal
    println("debt: put checking back in for unstackified things!")
    (hamuts6, locals0, BlockH(nodesByLine4, resultType), registerAccesses.last)
  }
}
