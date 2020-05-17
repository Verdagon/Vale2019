package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.metal._
import net.verdagon.vale.templar.Block2
import net.verdagon.vale.{vassert, vfail}

object BlockHammer {
  def translateBlock(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    initialLocals: Locals,
    stackHeight: StackHeightBox,
    block2: Block2):
  (BlockH, Option[RegisterAccessH[ReferendH]]) = {
    val locals = LocalsBox(initialLocals)

    // This means the caller didn't make us a new stack height.
    vassert(stackHeight.localsHeight == stackHeight.blockStartLocalsHeight)

    val nodesByLine = NodesBox(Vector[NodeH]());

    val (registerAccesses, deferreds) =
      ExpressionHammer.translateMaybeReturningExpressions(
        hinputs, hamuts, locals, stackHeight, nodesByLine, block2.exprs);

    vassert(deferreds.isEmpty) // curious, do we have to do any here

    val localIdsInThisBlock = locals.locals.keys.toSet.diff(initialLocals.locals.keys.toSet)
    vassert(stackHeight.localsHeight - stackHeight.blockStartLocalsHeight == localIdsInThisBlock.size)
    val localsInThisBlock = localIdsInThisBlock.map(locals.locals)
    vassert(localsInThisBlock.map(_.height).size == localsInThisBlock.size)
    val unstackifiedLocalIdsInThisBlock = locals.unstackifiedVars.intersect(localIdsInThisBlock)

//    if (localIdsInThisBlock != unstackifiedLocalIdsInThisBlock) {
//      // This probably means that there was no UnletH or DestructureH for that variable.
//      vfail("Ununstackified local: " + (localIdsInThisBlock -- unstackifiedLocalIdsInThisBlock))
//    }

    val resultType = registerAccesses.last.map(_.expectedType).getOrElse(ReferenceH(m.ShareH, VoidH()))
//    start here, we're returning locals and thats not optimal
    println("debt: put checking back in for unstackified things!")
    (BlockH(nodesByLine.inner, resultType), registerAccesses.last)
  }
}
