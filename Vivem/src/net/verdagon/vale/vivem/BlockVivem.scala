package net.verdagon.vale.vivem

import net.verdagon.vale.hammer.{Block3, Program3, Reference3, Void3}
import net.verdagon.vale.templar.types.{Ownership, Raw, Share}
import net.verdagon.vale.{vassert, vfail}
import net.verdagon.vale.vivem.ExpressionVivem.{NodeContinue, NodeReturn}

sealed trait IBlockExecuteResult
case class BlockContinue(resultRef: Option[ReturnV]) extends IBlockExecuteResult
// None means Void3
case class BlockReturn(returnRef: Option[ReturnV]) extends IBlockExecuteResult

object BlockVivem {
  def executeBlock(
    program3: Program3,
    stdin: (() => String),
    stdout: (String => Unit),
    heap: Heap,
    callId: CallId,
    block3: Block3
  ): IBlockExecuteResult = {
    val blockId = heap.pushNewBlock(callId)

    heap.vivemDout.println()
    heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame")

    val result = executeBlockInner(program3, stdin, stdout, heap, blockId, block3)

    heap.popBlock(blockId)
    heap.vivemDout.println()

    heap.vivemDout.print("  " * blockId.blockHeight + "Getting block result reference")

    result
  }

  def executeBlockInner(
    program3: Program3,
    stdin: (() => String),
    stdout: (String => Unit),
    heap: Heap,
    blockId: BlockId,
    block3: Block3
  ): IBlockExecuteResult = {
    val registersById = block3.nodes.groupBy(_.registerId).mapValues(_.head)
    vassert(registersById.size == block3.nodes.size)

    var currentLine = 0

    while (true) {
      val node = block3.nodes(currentLine)
      val registerId = node.registerId
      heap.vivemDout.print("  " * blockId.blockHeight + registerId + " " + node)
      heap.vivemDout.flush()

      val maybeResultRegisterId =
        ExpressionVivem.executeNode(program3, stdin, stdout, heap, blockId, node) match {
          case NodeContinue(r) => r
          case NodeReturn(maybeRet) => {
            return BlockReturn(maybeRet)
          }
        }

      currentLine = currentLine + 1
      vassert(currentLine <= block3.nodes.size)
      if (currentLine == block3.nodes.size) {
        val blockResult =
          (maybeResultRegisterId, block3.resultType) match {
            case (None, Reference3(Raw, Void3())) => {
              BlockContinue(None)
            }
            case (Some(resultRegisterId), expectedType) => {
              val ref = heap.returnFromRegister(resultRegisterId, expectedType)
              BlockContinue(Some(ref))
            }
          }
        return blockResult
      } else {
        heap.vivemDout.println()
      }
    }

    vfail() // Shouldnt get here
  }

}
