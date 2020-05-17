package net.verdagon.vale.vivem

import net.verdagon.vale.metal._
import net.verdagon.vale.{vimpl, metal => m}

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

  def getExternFunction(programH: ProgramH, ref: PrototypeH): (AdapterForExterns, Vector[ReferenceV]) => Option[ReturnV] = {
    ref.fullName.toString match {
      case """F("__addIntInt",[],[R(*,i),R(*,i)])""" => VivemExterns.addIntInt
      case """F("__addFloatFloat",[],[R(*,f),R(*,f)])""" => VivemExterns.addFloatFloat
//      case PrototypeH(FullNameH(List(NamePartH("panic", Some(List()), Some(List()), None))), List(), ReferenceH(m.Share, NeverH())) =>
//        VivemExterns.panic
      case """F("__multiplyIntInt",[],[R(*,i),R(*,i)])""" => VivemExterns.multiplyIntInt
//      case PrototypeH(FullNameH(List(NamePartH("__multiplyFloatFloat", Some(List()), Some(List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH()))), None))), List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH())), ReferenceH(m.Share,FloatH())) =>
//        VivemExterns.multiplyFloatFloat
      case """F("__subtractIntInt",[],[R(*,i),R(*,i)])""" => VivemExterns.subtractIntInt
//      case PrototypeH(FullNameH(List(NamePartH("__subtractFloatFloat", Some(List()), Some(List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH()))), None))), List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH())), ReferenceH(m.Share,FloatH())) =>
//        VivemExterns.subtractFloatFloat
      case """F("__addStrStr",[],[R(*,s),R(*,s)])""" => VivemExterns.addStrStr
//      case PrototypeH(FullNameH(List(NamePartH("__getch", Some(List()), Some(List()), None))),List(),ReferenceH(m.Share,IntH())) =>
//        VivemExterns.getch
//      case PrototypeH(FullNameH(List(NamePartH("__sqrt", Some(List()), Some(List(ReferenceH(m.Share,FloatH()))), None))),List(ReferenceH(m.Share,FloatH())),ReferenceH(m.Share,FloatH())) =>
//        VivemExterns.sqrt
//      case PrototypeH(FullNameH(List(NamePartH("__lessThanInt", Some(List()), Some(List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH()))), None))), List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.lessThanInt
//      case PrototypeH(FullNameH(List(NamePartH("__lessThanFloat", Some(List()), Some(List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH()))), None))), List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.lessThanFloat
//      case PrototypeH(FullNameH(List(NamePartH("__greaterThanFloat", Some(List()), Some(List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH()))), None))), List(ReferenceH(m.Share,FloatH()), ReferenceH(m.Share,FloatH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.greaterThanFloat
//      case PrototypeH(FullNameH(List(NamePartH("__lessThanOrEqInt", Some(List()), Some(List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH()))), None))), List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.lessThanOrEqInt
//      case PrototypeH(FullNameH(List(NamePartH("__greaterThanInt", Some(List()), Some(List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH()))), None))), List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.greaterThanInt
//      case PrototypeH(FullNameH(List(NamePartH("__greaterThanOrEqInt", Some(List()), Some(List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH()))), None))), List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.greaterThanOrEqInt
      case """F("__eqIntInt",[],[R(*,i),R(*,i)])""" => VivemExterns.eqIntInt
//      case PrototypeH(FullNameH(List(NamePartH("__eqBoolBool", Some(List()), Some(List(ReferenceH(m.Share,BoolH()), ReferenceH(m.Share,BoolH()))), None))), List(ReferenceH(m.Share,BoolH()), ReferenceH(m.Share,BoolH())), ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.eqBoolBool
      case """F("__print",[],[R(*,s)])""" => VivemExterns.print
//      case PrototypeH(FullNameH(List(NamePartH("__not", Some(List()), Some(List(ReferenceH(m.Share,BoolH()))), None))),List(ReferenceH(m.Share,BoolH())),ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.not
      case """F("__castIntStr",[],[R(*,i)])""" => VivemExterns.castIntStr
//      case PrototypeH(FullNameH(List(NamePartH("__castFloatStr", Some(List()), Some(List(ReferenceH(m.Share,FloatH()))), None))),List(ReferenceH(m.Share,FloatH())),ReferenceH(m.Share,StrH())) =>
//        VivemExterns.castFloatStr
//      case PrototypeH(FullNameH(List(NamePartH("__castIntFloat", Some(List()), Some(List(ReferenceH(m.Share,IntH()))), None))),List(ReferenceH(m.Share,IntH())),ReferenceH(m.Share,FloatH())) =>
//        VivemExterns.castIntFloat
//      case PrototypeH(FullNameH(List(NamePartH("__and", Some(List()), Some(List(ReferenceH(m.Share,BoolH()), ReferenceH(m.Share,BoolH()))), None))),List(ReferenceH(m.Share,BoolH()), ReferenceH(m.Share,BoolH())),ReferenceH(m.Share,BoolH())) =>
//        VivemExterns.and
//      case PrototypeH(FullNameH(List(NamePartH("__mod", Some(List()), Some(List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH()))), None))),List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())),ReferenceH(m.Share,IntH())) =>
//        VivemExterns.mod
      case _ => vimpl(ref.fullName.toString)
    }
  }
}
