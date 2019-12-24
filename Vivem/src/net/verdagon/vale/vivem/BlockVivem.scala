package net.verdagon.vale.vivem

import net.verdagon.vale.hammer.{BlockH, ProgramH, ReferenceH, VoidH}
import net.verdagon.vale.templar.types.{Ownership, Raw, Share}
import net.verdagon.vale.{vassert, vfail}
import net.verdagon.vale.vivem.ExpressionVivem.{NodeContinue, NodeReturn}

sealed trait IBlockExecuteResult
case class BlockContinue(resultRef: Option[ReturnV]) extends IBlockExecuteResult
// None means VoidH
case class BlockReturn(returnRef: Option[ReturnV]) extends IBlockExecuteResult

object BlockVivem {
  def executeBlock(
    programH: ProgramH,
    stdin: (() => String),
    stdout: (String => Unit),
    heap: Heap,
    callId: CallId,
    blockH: BlockH
  ): IBlockExecuteResult = {
    val blockId = heap.pushNewBlock(callId)

    heap.vivemDout.println()
    heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame")

    val result = executeBlockInner(programH, stdin, stdout, heap, blockId, blockH)

    heap.popBlock(blockId)
    heap.vivemDout.println()

    heap.vivemDout.print("  " * blockId.blockHeight + "Getting block result reference")

    result
  }

  def executeBlockInner(
    programH: ProgramH,
    stdin: (() => String),
    stdout: (String => Unit),
    heap: Heap,
    blockId: BlockId,
    blockH: BlockH
  ): IBlockExecuteResult = {
    val registersById = blockH.nodes.groupBy(_.registerId).mapValues(_.head)
    vassert(registersById.size == blockH.nodes.size)

    var currentLine = 0

    while (true) {
      val node = blockH.nodes(currentLine)
      val registerId = node.registerId
      heap.vivemDout.print("  " * blockId.blockHeight + registerId + " " + node)
      heap.vivemDout.flush()

      val maybeResultRegisterId =
        ExpressionVivem.executeNode(programH, stdin, stdout, heap, blockId, node) match {
          case NodeContinue(r) => r
          case NodeReturn(maybeRet) => {
            return BlockReturn(maybeRet)
          }
        }

      currentLine = currentLine + 1
      vassert(currentLine <= blockH.nodes.size)
      if (currentLine == blockH.nodes.size) {
        val blockResult =
          (maybeResultRegisterId, blockH.resultType) match {
            case (None, ReferenceH(Raw, VoidH())) => {
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
