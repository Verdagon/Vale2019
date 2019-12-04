package net.verdagon.radonc.hammer

import net.verdagon.radonc.hinputs.Hinputs
import net.verdagon.radonc.templar.Block2
import net.verdagon.radonc.templar.types.Raw
import net.verdagon.radonc.{vassert, vfail}

object BlockHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def addLine(nodesByLine: Vector[Node3], node: Node3): (Vector[Node3], Node3) = {
    (nodesByLine :+ node, node)
  }

  def translateBlock(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    newStackHeight: StackHeight,
    block2: Block2):
  (Hamuts, Locals, Block3, Option[RegisterAccess3[Referend3]]) = {

    // This means the caller didn't make us a new stack height.
    vassert(newStackHeight.localsHeight == newStackHeight.blockStartLocalsHeight)

    val nodesByLine0 = Vector[Node3]();

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
//      // This probably means that there was no Unlet3 or Destructure3 for that variable.
//      vfail("Ununstackified local: " + (localIdsInThisBlock -- unstackifiedLocalIdsInThisBlock))
//    }

    val resultType = registerAccesses.last.map(_.expectedType).getOrElse(Reference3(Raw, Void3()))
//    start here, we're returning locals0 and thats not optimal
    println("debt: put checking back in for unstackified things!")
    (hamuts6, locals0, Block3(nodesByLine4, resultType), registerAccesses.last)
  }
}
