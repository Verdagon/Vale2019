package net.verdagon.radonc.vivem

import java.io.PrintStream

import net.verdagon.radonc.hammer.{Program3, Void3}
import net.verdagon.radonc.templar.types.Share
import net.verdagon.radonc.{vassert, vfail}
import net.verdagon.von.IVonData

case class PanicException() extends Throwable

object Vivem {
  def executeWithPrimitiveArgs(
      program3: Program3,
      externalArgumentReferends: Vector[PrimitiveReferendV],
      vivemDout: PrintStream,
      stdin: () => String,
      stdout: String => Unit): Option[IVonData] = {
    val heap = new Heap(vivemDout)
    val argReferences =
      externalArgumentReferends.map(argReferend => {
        heap.add(Share, argReferend);
      });
    innerExecute(program3, argReferences, heap, vivemDout, stdin, stdout)
  }

  def executeWithHeap(
      program3: Program3,
      inputHeap: Heap,
      inputArgumentReferences: Vector[ReferenceV],
      vivemDout: PrintStream,
      stdin: () => String,
      stdout: String => Unit):
  Option[IVonData] = {
    vassert(inputHeap.countUnreachableAllocations(inputArgumentReferences) == 0)
    innerExecute(program3, inputArgumentReferences, inputHeap, vivemDout, stdin, stdout)
  }

  def emptyStdin() = {
    vfail("Empty stdin!")
  }

  def nullStdout(str: String) = {
  }
  def regularStdout(str: String) = {
    print(str)
  }

  def stdinFromList(stdinList: List[String]) = {
    var remainingStdin = stdinList
    val stdin = (() => {
      vassert(remainingStdin.nonEmpty)
      val result = remainingStdin.head
      remainingStdin = remainingStdin.tail
      result
    })
    stdin
  }

  def stdoutCollector(): (StringBuilder, String => Unit) = {
    val stdoutput = new StringBuilder()
    val func = (str: String) => { print(str); stdoutput.append(str); }: Unit
    (stdoutput, func)
  }

  def innerExecute(
      program3: Program3,
      argumentReferences: Vector[ReferenceV],
      heap: Heap,
      vivemDout: PrintStream,
      stdin: () => String,
      stdout: String => Unit): Option[IVonData] = {
    val main = program3.main

    val callId = CallId(0, main)
    val blockId = BlockId(callId, 0)

    vivemDout.print("Making stack frame")
    vivemDout.println()

    val maybeReturnRef =
      FunctionVivem.executeFunction(program3, stdin, stdout, heap, argumentReferences, main)

    vivemDout.print("Ending program")
    (main.prototype.returnType.kind, maybeReturnRef) match {
      case (Void3(), None) => {
        vivemDout.println()
        println("Checking for leaks")
        heap.checkForLeaks()
        vivemDout.println()
        None
      }
      case (_, Some(returnRef)) => {
        heap.decrementReferenceRefCount(
          ResultToObjectReferrer(blockId.callId),
          returnRef.reference)
        val von = heap.toVon(returnRef.reference)
        ExpressionVivem.dropReferenceIfNonOwning(
          program3, heap, stdout, stdin, blockId, returnRef.reference)
        vivemDout.println()
        println("Checking for leaks")
        heap.checkForLeaks()
        vivemDout.println()
        Some(von)
      }
    }
  }
}
