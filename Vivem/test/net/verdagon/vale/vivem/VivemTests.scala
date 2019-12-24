package net.verdagon.vale.vivem

import net.verdagon.vale.hammer._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules.{CoordTypeSR, TypedSR}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{ReferenceLocalVariable2, VariableId2}
import net.verdagon.vale.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.vale.templar.types._
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class VivemTests extends FunSuite with Matchers {
  test("Adding") {

    val main =
      FunctionH(
        PrototypeH(0,FullNameH(List(NamePartH("main", Some(List())))),List(),ReferenceH(Share,IntH())),
        false,
        false,
        true,
        BlockH(
          Vector(
            ConstantI64H("0",52),
            ConstantI64H("1",53),
            ExternCallH("2", FunctionRefH(PrototypeH(1000,FullNameH(List(NamePartH("__addIntInt", Some(List())))),List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())),ReferenceH(Share,IntH()))), List(RegisterAccessH("0",ReferenceH(Share,IntH())), RegisterAccessH("1",ReferenceH(Share,IntH())))),
            ConstantI64H("3",54),
            ExternCallH("4", FunctionRefH(PrototypeH(1000,FullNameH(List(NamePartH("__addIntInt", Some(List())))),List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())),ReferenceH(Share,IntH()))), List(RegisterAccessH("2",ReferenceH(Share,IntH())), RegisterAccessH("3",ReferenceH(Share,IntH())))),
            ConstantI64H("5",55),
            ExternCallH("6", FunctionRefH(PrototypeH(1000,FullNameH(List(NamePartH("__addIntInt", Some(List())))),List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())),ReferenceH(Share,IntH()))), List(RegisterAccessH("4",ReferenceH(Share,IntH())), RegisterAccessH("5",ReferenceH(Share,IntH())))),
            ConstantI64H("7",56),
            ExternCallH("8", FunctionRefH(PrototypeH(1000,FullNameH(List(NamePartH("__addIntInt", Some(List())))),List(ReferenceH(Share,IntH()), ReferenceH(Share,IntH())),ReferenceH(Share,IntH()))), List(RegisterAccessH("6",ReferenceH(Share,IntH())), RegisterAccessH("7",ReferenceH(Share,IntH()))))),
          ReferenceH(Share,IntH())))
    val programH =
      ProgramH(List(), List(), StructRefH(0,FullNameH(List(NamePartH("__Pack", Some(List()))))), List(), List(main))
    val result =
      Vivem.executeWithPrimitiveArgs(programH, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    result shouldEqual Some(VonInt(270))
  }
}
