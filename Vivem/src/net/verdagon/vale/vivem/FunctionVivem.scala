package net.verdagon.vale.vivem

import net.verdagon.vale.hammer._
import net.verdagon.vale.templar.types.{Raw, Share}

object FunctionVivem {
  def executeFunction(
      program3: Program3,
      stdin: (() => String),
      stdout: (String => Unit),
      heap: Heap,
      args: Vector[ReferenceV],
      function3: Function3
  ): Option[ReturnV] = {
    val callId = heap.pushNewStackFrame(function3, args)

//    heap.vivemDout.println("About to execute:")
//    function3.nodes.foreach(heap.vivemDout.println)
//    heap.vivemDout.println("/Function")

    heap.vivemDout.print("  " * callId.blockDepth + "Entering function " + callId)

    // Increment all the args to show that they have arguments referring to them.
    // These will be decremented at some point in the callee function.
    args.indices.foreach(argIndex => {
      heap.incrementReferenceRefCount(
        ArgumentToObjectReferrer(ArgumentId(callId, argIndex)),
        args(argIndex))
    })

    heap.vivemDout.println()

    val maybeReturn =
      BlockVivem.executeBlock(program3, stdin, stdout, heap, callId, function3.block) match {
        case BlockContinue(maybeRet) => maybeRet
        case BlockReturn(maybeRet) => maybeRet
      }

    heap.vivemDout.println()
    heap.vivemDout.print("  " * callId.blockDepth + "Returning")

    heap.popStackFrame(callId)

    heap.vivemDout.println()

    maybeReturn
  }

  def getExternFunction(program3: Program3, ref: FunctionRef3): (AdapterForExterns, Vector[ReferenceV]) => Option[ReturnV] = {
    ref.prototype match {
      case Prototype3(_, FullName3(List(NamePart3("__addIntInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Int3())) =>
        VivemExterns.addIntInt
      case Prototype3(_, FullName3(List(NamePart3("__addFloatFloat", Some(List())))), List(Reference3(Share,Float3()), Reference3(Share,Float3())), Reference3(Share,Float3())) =>
        VivemExterns.addFloatFloat
      case Prototype3(_, FullName3(List(NamePart3("panic", Some(List())))), List(), Reference3(Raw,Never3())) =>
        VivemExterns.panic
      case Prototype3(_, FullName3(List(NamePart3("__multiplyIntInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Int3())) =>
        VivemExterns.multiplyIntInt
      case Prototype3(_, FullName3(List(NamePart3("__multiplyFloatFloat", Some(List())))), List(Reference3(Share,Float3()), Reference3(Share,Float3())), Reference3(Share,Float3())) =>
        VivemExterns.multiplyFloatFloat
      case Prototype3(_, FullName3(List(NamePart3("__subtractIntInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Int3())) =>
        VivemExterns.subtractIntInt
      case Prototype3(_, FullName3(List(NamePart3("__subtractFloatFloat", Some(List())))), List(Reference3(Share,Float3()), Reference3(Share,Float3())), Reference3(Share,Float3())) =>
        VivemExterns.subtractFloatFloat
      case Prototype3(_, FullName3(List(NamePart3("__addStrStr", Some(List())))), List(Reference3(Share,Str3()), Reference3(Share,Str3())), Reference3(Share,Str3())) =>
        VivemExterns.addStrStr
      case Prototype3(_, FullName3(List(NamePart3("__getch", Some(List())))),List(),Reference3(Share,Int3())) =>
        VivemExterns.getch
      case Prototype3(_, FullName3(List(NamePart3("__sqrt", Some(List())))),List(Reference3(Share,Float3())),Reference3(Share,Float3())) =>
        VivemExterns.sqrt
      case Prototype3(_, FullName3(List(NamePart3("__lessThanInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Bool3())) =>
        VivemExterns.lessThanInt
      case Prototype3(_, FullName3(List(NamePart3("__lessThanFloat", Some(List())))), List(Reference3(Share,Float3()), Reference3(Share,Float3())), Reference3(Share,Bool3())) =>
        VivemExterns.lessThanFloat
      case Prototype3(_, FullName3(List(NamePart3("__greaterThanFloat", Some(List())))), List(Reference3(Share,Float3()), Reference3(Share,Float3())), Reference3(Share,Bool3())) =>
        VivemExterns.greaterThanFloat
      case Prototype3(_, FullName3(List(NamePart3("__lessThanOrEqInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Bool3())) =>
        VivemExterns.lessThanOrEqInt
      case Prototype3(_, FullName3(List(NamePart3("__greaterThanInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Bool3())) =>
        VivemExterns.greaterThanInt
      case Prototype3(_, FullName3(List(NamePart3("__greaterThanOrEqInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Bool3())) =>
        VivemExterns.greaterThanOrEqInt
      case Prototype3(_, FullName3(List(NamePart3("__eqIntInt", Some(List())))), List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Bool3())) =>
        VivemExterns.eqIntInt
      case Prototype3(_, FullName3(List(NamePart3("__eqBoolBool", Some(List())))), List(Reference3(Share,Bool3()), Reference3(Share,Bool3())), Reference3(Share,Bool3())) =>
        VivemExterns.eqBoolBool
      case Prototype3(_, FullName3(List(NamePart3("__print", Some(List())))),List(Reference3(Share,Str3())), Reference3(Raw, Void3())) =>
        VivemExterns.print
      case Prototype3(_, FullName3(List(NamePart3("__not", Some(List())))),List(Reference3(Share,Bool3())),Reference3(Share,Bool3())) =>
        VivemExterns.not
      case Prototype3(_, FullName3(List(NamePart3("__castIntStr", Some(List())))),List(Reference3(Share,Int3())),Reference3(Share,Str3())) =>
        VivemExterns.castIntStr
      case Prototype3(_, FullName3(List(NamePart3("__castFloatStr", Some(List())))),List(Reference3(Share,Float3())),Reference3(Share,Str3())) =>
        VivemExterns.castFloatStr
      case Prototype3(_, FullName3(List(NamePart3("__castIntFloat", Some(List())))),List(Reference3(Share,Int3())),Reference3(Share,Float3())) =>
        VivemExterns.castIntFloat
      case Prototype3(_, FullName3(List(NamePart3("__and", Some(List())))),List(Reference3(Share,Bool3()), Reference3(Share,Bool3())),Reference3(Share,Bool3())) =>
        VivemExterns.and
      case Prototype3(_, FullName3(List(NamePart3("__mod", Some(List())))),List(Reference3(Share,Int3()), Reference3(Share,Int3())),Reference3(Share,Int3())) =>
        VivemExterns.mod
    }
  }
}
