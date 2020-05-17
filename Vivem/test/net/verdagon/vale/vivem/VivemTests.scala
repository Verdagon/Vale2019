package net.verdagon.vale.vivem

import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.von.{VonArray, VonInt, VonMember, VonObject, VonStr}
import org.scalatest.{FunSuite, Matchers}

class VivemTests extends FunSuite with Matchers {
  test("Adding") {

    val addPrototype =
      PrototypeH(
        FullNameH(List(VonObject("F",None,Vector(VonMember(None,Some("humanName"),VonStr("__addIntInt")), VonMember(None,Some("templateArgs"),VonArray(None,Vector())), VonMember(None,Some("parameters"),VonArray(None,Vector(VonObject("Ref",None,Vector(VonMember(None,Some("ownership"),VonObject("Share",None,Vector())), VonMember(None,Some("kind"),VonObject("Int",None,Vector())))), VonObject("Ref",None,Vector(VonMember(None,Some("ownership"),VonObject("Share",None,Vector())), VonMember(None,Some("kind"),VonObject("Int",None,Vector()))))))))))),
        List(ReferenceH(ShareH,IntH()), ReferenceH(ShareH,IntH())),
        ReferenceH(ShareH,IntH()))
    val main =
      FunctionH(
        PrototypeH(FullNameH(List(VonObject("F",None,Vector(VonMember(None,Some("humanName"),VonStr("main")), VonMember(None,Some("templateArgs"),VonArray(None,Vector())), VonMember(None,Some("parameters"),VonArray(None,Vector())))))),List(),ReferenceH(m.ShareH,IntH())),
        false,
        false,
        true,
        BlockH(
          Vector(
            ConstantI64H("0",52),
            ConstantI64H("1",53),
            CallH("2", addPrototype, List(RegisterAccessH("0",ReferenceH(m.ShareH,IntH())), RegisterAccessH("1",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("3",54),
            CallH("4", addPrototype, List(RegisterAccessH("2",ReferenceH(m.ShareH,IntH())), RegisterAccessH("3",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("5",55),
            CallH("6", addPrototype, List(RegisterAccessH("4",ReferenceH(m.ShareH,IntH())), RegisterAccessH("5",ReferenceH(m.ShareH,IntH())))),
            ConstantI64H("7",56),
            CallH("8", addPrototype, List(RegisterAccessH("6",ReferenceH(m.ShareH,IntH())), RegisterAccessH("7",ReferenceH(m.ShareH,IntH()))))),
          ReferenceH(m.ShareH,IntH())))
    val addExtern =
      FunctionH(
        addPrototype,
        false,
        true,
        false,
        BlockH(Vector(ConstantI64H("0", 133337)), ReferenceH(m.ShareH,IntH())))
    val programH =
      ProgramH(
        List(),
        List(),
        StructRefH(FullNameH(List(VonStr("__Pack"),
          VonArray(None,
            Vector())))),
        List(),
        List(main, addExtern))
    val result =
      Vivem.executeWithPrimitiveArgs(programH, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    result shouldEqual Some(VonInt(270))
  }
}
