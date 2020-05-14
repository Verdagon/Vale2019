package net.verdagon.vale.vivem

import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.von.{VonArray, VonInt, VonStr}
import org.scalatest.{FunSuite, Matchers}

class VivemTests extends FunSuite with Matchers {
  test("Adding") {

    val main =
      FunctionH(
        PrototypeH(FullNameH(VonArray(None, Vector(VonStr("[F(\"main\")]")))),List(),ReferenceH(m.Share,IntH())),
        false,
        false,
        true,
        BlockH(
          Vector(
            ConstantI64H("0",52),
            ConstantI64H("1",53),
            CallH("2", PrototypeH(FullNameH(VonArray(None, Vector(VonStr("__addIntInt")))),List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())),ReferenceH(m.Share,IntH())), List(RegisterAccessH("0",ReferenceH(m.Share,IntH())), RegisterAccessH("1",ReferenceH(m.Share,IntH())))),
            ConstantI64H("3",54),
            CallH("4", PrototypeH(FullNameH(VonArray(None, Vector(VonStr("__addIntInt")))),List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())),ReferenceH(m.Share,IntH())), List(RegisterAccessH("2",ReferenceH(m.Share,IntH())), RegisterAccessH("3",ReferenceH(m.Share,IntH())))),
            ConstantI64H("5",55),
            CallH("6", PrototypeH(FullNameH(VonArray(None, Vector(VonStr("__addIntInt")))),List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())),ReferenceH(m.Share,IntH())), List(RegisterAccessH("4",ReferenceH(m.Share,IntH())), RegisterAccessH("5",ReferenceH(m.Share,IntH())))),
            ConstantI64H("7",56),
            CallH("8", PrototypeH(FullNameH(VonArray(None, Vector(VonStr("__addIntInt")))),List(ReferenceH(m.Share,IntH()), ReferenceH(m.Share,IntH())),ReferenceH(m.Share,IntH())), List(RegisterAccessH("6",ReferenceH(m.Share,IntH())), RegisterAccessH("7",ReferenceH(m.Share,IntH()))))),
          ReferenceH(m.Share,IntH())))
    val programH =
      ProgramH(List(), List(), StructRefH(FullNameH(VonArray(None, Vector(VonStr("__Pack"), VonArray(None, Vector()))))), List(), List(main))
    val result =
      Vivem.executeWithPrimitiveArgs(programH, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    result shouldEqual Some(VonInt(270))
  }
}
