package net.verdagon.vale.vivem

import net.verdagon.vale.hammer._
import net.verdagon.vale.templar.types.{Raw, Share}

object FunctionVivem {
  def executeFunction(
      programH: ProgramH,
      stdin: (() => String),
      stdout: (String => Unit),
      heap: Heap,
      args: Vector[ReferenceV],
      functionH: FunctionH
  ): Option[ReturnV] = {
    val callId = heap.pushNewStackFrame(functionH, args)

//    heap.vivemDout.println("About to execute:")
//    functionH.nodes.foreach(heap.vivemDout.println)
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
      BlockVivem.executeBlock(programH, stdin, stdout, heap, callId, functionH.block) match {
        case BlockContinue(maybeRet) => maybeRet
        case BlockReturn(maybeRet) => maybeRet
      }

    heap.vivemDout.println()
    heap.vivemDout.print("  " * callId.blockDepth + "Returning")

    heap.popStackFrame(callId)

    heap.vivemDout.println()

    maybeReturn
  }

  def getExternFunction(programH: ProgramH, ref: FunctionRefH): (AdapterForExterns, Vector[ReferenceV]) => Option[ReturnV] = {
    ref.prototype match {
      case PrototypeH(_, FullNameH(List(NamePartH("__addIntInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,IntH())) =>
        VivemExterns.addIntInt
      case PrototypeH(_, FullNameH(List(NamePartH("__addFloatFloat", Some(List())))), List(ReferenceH(Share,FloatH()), ReferenceH(Share,FloatH())), ReferenceH(Share,FloatH())) =>
        VivemExterns.addFloatFloat
      case PrototypeH(_, FullNameH(List(NamePartH("panic", Some(List())))), List(), ReferenceH(Raw,NeverH())) =>
        VivemExterns.panic
      case PrototypeH(_, FullNameH(List(NamePartH("__multiplyIntInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,IntH())) =>
        VivemExterns.multiplyIntInt
      case PrototypeH(_, FullNameH(List(NamePartH("__multiplyFloatFloat", Some(List())))), List(ReferenceH(Share,FloatH()), ReferenceH(Share,FloatH())), ReferenceH(Share,FloatH())) =>
        VivemExterns.multiplyFloatFloat
      case PrototypeH(_, FullNameH(List(NamePartH("__subtractIntInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,IntH())) =>
        VivemExterns.subtractIntInt
      case PrototypeH(_, FullNameH(List(NamePartH("__subtractFloatFloat", Some(List())))), List(ReferenceH(Share,FloatH()), ReferenceH(Share,FloatH())), ReferenceH(Share,FloatH())) =>
        VivemExterns.subtractFloatFloat
      case PrototypeH(_, FullNameH(List(NamePartH("__addStrStr", Some(List())))), List(ReferenceH(Share,StrH()), ReferenceH(Share,StrH())), ReferenceH(Share,StrH())) =>
        VivemExterns.addStrStr
      case PrototypeH(_, FullNameH(List(NamePartH("__getch", Some(List())))),List(),ReferenceH(Share,IntH())) =>
        VivemExterns.getch
      case PrototypeH(_, FullNameH(List(NamePartH("__sqrt", Some(List())))),List(ReferenceH(Share,FloatH())),ReferenceH(Share,FloatH())) =>
        VivemExterns.sqrt
      case PrototypeH(_, FullNameH(List(NamePartH("__lessThanInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.lessThanInt
      case PrototypeH(_, FullNameH(List(NamePartH("__lessThanFloat", Some(List())))), List(ReferenceH(Share,FloatH()), ReferenceH(Share,FloatH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.lessThanFloat
      case PrototypeH(_, FullNameH(List(NamePartH("__greaterThanFloat", Some(List())))), List(ReferenceH(Share,FloatH()), ReferenceH(Share,FloatH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.greaterThanFloat
      case PrototypeH(_, FullNameH(List(NamePartH("__lessThanOrEqInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.lessThanOrEqInt
      case PrototypeH(_, FullNameH(List(NamePartH("__greaterThanInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.greaterThanInt
      case PrototypeH(_, FullNameH(List(NamePartH("__greaterThanOrEqInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.greaterThanOrEqInt
      case PrototypeH(_, FullNameH(List(NamePartH("__eqIntInt", Some(List())))), List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.eqIntInt
      case PrototypeH(_, FullNameH(List(NamePartH("__eqBoolBool", Some(List())))), List(ReferenceH(Share,BoolH()), ReferenceH(Share,BoolH())), ReferenceH(Share,BoolH())) =>
        VivemExterns.eqBoolBool
      case PrototypeH(_, FullNameH(List(NamePartH("__print", Some(List())))),List(ReferenceH(Share,StrH())), ReferenceH(Raw, VoidH())) =>
        VivemExterns.print
      case PrototypeH(_, FullNameH(List(NamePartH("__not", Some(List())))),List(ReferenceH(Share,BoolH())),ReferenceH(Share,BoolH())) =>
        VivemExterns.not
      case PrototypeH(_, FullNameH(List(NamePartH("__castIntStr", Some(List())))),List(ReferenceH(Share,IntH())),ReferenceH(Share,StrH())) =>
        VivemExterns.castIntStr
      case PrototypeH(_, FullNameH(List(NamePartH("__castFloatStr", Some(List())))),List(ReferenceH(Share,FloatH())),ReferenceH(Share,StrH())) =>
        VivemExterns.castFloatStr
      case PrototypeH(_, FullNameH(List(NamePartH("__castIntFloat", Some(List())))),List(ReferenceH(Share,IntH())),ReferenceH(Share,FloatH())) =>
        VivemExterns.castIntFloat
      case PrototypeH(_, FullNameH(List(NamePartH("__and", Some(List())))),List(ReferenceH(Share,BoolH()), ReferenceH(Share,BoolH())),ReferenceH(Share,BoolH())) =>
        VivemExterns.and
      case PrototypeH(_, FullNameH(List(NamePartH("__mod", Some(List())))),List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())),ReferenceH(Share,IntH())) =>
        VivemExterns.mod
    }
  }
}
