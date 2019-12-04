package net.verdagon.radonc.vivem

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules.{CoordTypeSR, TypedSR}
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.radonc.templar.types._
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class VivemTests extends FunSuite with Matchers {
  test("Adding") {

    val main =
      Function3(
        Prototype3(0,FullName3(List(NamePart3("main", Some(List())))),List(),Reference3(Share,Int3())),
        false,
        false,
        true,
        Block3(
          Vector(
            ConstantI643("0",52),
            ConstantI643("1",53),
            ExternCall3("2", FunctionRef3(Prototype3(1000,FullName3(List(NamePart3("__addIntInt", Some(List())))),List(Reference3(Share,Int3()), Reference3(Share,Int3())),Reference3(Share,Int3()))), List(RegisterAccess3("0",Reference3(Share,Int3())), RegisterAccess3("1",Reference3(Share,Int3())))),
            ConstantI643("3",54),
            ExternCall3("4", FunctionRef3(Prototype3(1000,FullName3(List(NamePart3("__addIntInt", Some(List())))),List(Reference3(Share,Int3()), Reference3(Share,Int3())),Reference3(Share,Int3()))), List(RegisterAccess3("2",Reference3(Share,Int3())), RegisterAccess3("3",Reference3(Share,Int3())))),
            ConstantI643("5",55),
            ExternCall3("6", FunctionRef3(Prototype3(1000,FullName3(List(NamePart3("__addIntInt", Some(List())))),List(Reference3(Share,Int3()), Reference3(Share,Int3())),Reference3(Share,Int3()))), List(RegisterAccess3("4",Reference3(Share,Int3())), RegisterAccess3("5",Reference3(Share,Int3())))),
            ConstantI643("7",56),
            ExternCall3("8", FunctionRef3(Prototype3(1000,FullName3(List(NamePart3("__addIntInt", Some(List())))),List(Reference3(Share,Int3()), Reference3(Share,Int3())),Reference3(Share,Int3()))), List(RegisterAccess3("6",Reference3(Share,Int3())), RegisterAccess3("7",Reference3(Share,Int3()))))),
          Reference3(Share,Int3())))
    val program3 =
      Program3(List(), List(), StructRef3(0,FullName3(List(NamePart3("__Pack", Some(List()))))), List(), List(main))
    val result =
      Vivem.executeWithPrimitiveArgs(program3, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    result shouldEqual Some(VonInt(270))
  }
}
