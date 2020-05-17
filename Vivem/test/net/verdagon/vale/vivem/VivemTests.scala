package net.verdagon.vale.vivem

import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.von.{VonArray, VonInt, VonStr}
import org.scalatest.{FunSuite, Matchers}

class VivemTests extends FunSuite with Matchers {
  test("Adding") {

    val main =
      FunctionH(
        PrototypeH(FullNameH(List(VonStr("F(\"main\")"))),List(),ReferenceH(m.ShareH,IntH())),
        false,
        false,
        true,
        BlockH(
          Vector(
            ConstantI64H("0",52),
            ConstantI64H("1",53),
            CallH("2", PrototypeH(FullNameH(List(VonStr("__addIntInt"))),List(ReferenceH(m.ShareH,IntH()), ReferenceH(m.ShareH,IntH())),ReferenceH(m.ShareH,IntH())), List(RegisterAccessH("0",ReferenceH(m.ShareH,IntH())), RegisterAccessH("1",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("3",54),
            CallH("4", PrototypeH(FullNameH(List(VonStr("__addIntInt"))),List(ReferenceH(m.ShareH,IntH()), ReferenceH(m.ShareH,IntH())),ReferenceH(m.ShareH,IntH())), List(RegisterAccessH("2",ReferenceH(m.ShareH,IntH())), RegisterAccessH("3",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("5",55),
            CallH("6", PrototypeH(FullNameH(List(VonStr("__addIntInt"))),List(ReferenceH(m.ShareH,IntH()), ReferenceH(m.ShareH,IntH())),ReferenceH(m.ShareH,IntH())), List(RegisterAccessH("4",ReferenceH(m.ShareH,IntH())), RegisterAccessH("5",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("7",56),
            CallH("8", PrototypeH(FullNameH(List(VonStr("__addIntInt"))),List(ReferenceH(m.ShareH,IntH()), ReferenceH(m.ShareH,IntH())),ReferenceH(m.ShareH,IntH())), List(RegisterAccessH("6",ReferenceH(m.ShareH,IntH())), RegisterAccessH("7",ReferenceH(m.ShareH,IntH()))))),
          ReferenceH(m.ShareH,IntH())))
    val programH =
      ProgramH(List(), List(), StructRefH(FullNameH(List(VonStr("__Pack"), VonArray(None, Vector())))), List(), List(main))
    val result =
      Vivem.executeWithPrimitiveArgs(programH, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    result shouldEqual Some(VonInt(270))
  }
}
