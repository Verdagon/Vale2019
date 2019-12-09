package net.verdagon.radonc.templatetyper

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.radonc.scout.rules.{EqualsSR, _}
import net.verdagon.radonc.{vassert, vassertSome, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

case class FakeEnv()
case class FakeState()

case class SimpleEnvironment(entries: Map[String, List[ITemplataType]]) {
  def lookupType(name: String): ITemplataType = {
    val List(thing) = entries(name)
    thing
  }
}

class FakeRuleTyperEvaluatorDelegate extends IRuleTyperEvaluatorDelegate[SimpleEnvironment, FakeState] {
  override def lookupType(state: FakeState, env: SimpleEnvironment, name: String): (ITemplataType) = {
    (env.lookupType(name))
  }
}

class RuleTyperTests extends FunSuite with Matchers {
  def makeCannedEnvironment(): SimpleEnvironment = {
    SimpleEnvironment(
      Map(
        "ImmInterface" -> List(KindTemplataType),
        "__Array" -> List(TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
        "MutTStruct" -> List(TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
        "MutTInterface" -> List(TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
        "MutStruct" -> List(KindTemplataType),
        "MutInterface" -> List(KindTemplataType),
        "Void" -> List(KindTemplataType),
        "Int" -> List(KindTemplataType)))
  }

  def makeCannedRuleTyper(): RuleTyperEvaluator[SimpleEnvironment, FakeState] = {
    new RuleTyperEvaluator[SimpleEnvironment, FakeState](
      new FakeRuleTyperEvaluatorDelegate() {
        override def lookupType(state: FakeState, env: SimpleEnvironment, name: String): (ITemplataType) = {
          (env.lookupType(name))
        }
      })
  }

  def makeRuleTyper(
    maybeRuleTyperEvaluator: Option[RuleTyperEvaluator[SimpleEnvironment, FakeState]]):
  RuleTyperEvaluator[SimpleEnvironment, FakeState] = {
    val ruleTyperEvaluator =
      maybeRuleTyperEvaluator match {
        case None =>
          new RuleTyperEvaluator[SimpleEnvironment, FakeState](
            new FakeRuleTyperEvaluatorDelegate())
        case Some(x) => x
      }
    return ruleTyperEvaluator
  }

  test("Borrow becomes share if kind is immutable") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("__C"),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST("__C")),TemplexSR(OwnershippedST(BorrowP,NameST("ImmInterface"))))),
          List(),
          None)

    vassert(conclusions.typeByRune("__C") == CoordTemplataType)
  }

  test("Can infer coord rune from an incoming kind") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(TypedSR(Some("C"), CoordTypeSR)),
          List(),
          None)

    vassert(conclusions.typeByRune("C") == CoordTemplataType)
  }

  test("Detects conflict between types") {
    val (_, isf @ RuleTyperSolveFailure(_, _, _)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(EqualsSR(TypedSR(Some("C"), CoordTypeSR), TypedSR(Some("C"), KindTypeSR))),
          List(),
          None)

    vassert(isf.toString.contains("but previously concluded"))
  }

  test("Can explicitly coerce from kind to coord") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(EqualsSR(TypedSR(Some("C"), CoordTypeSR), CallSR("toRef", List(TypedSR(Some("A"), KindTypeSR))))),
          List(),
          None)

    conclusions.typeByRune("C") shouldEqual CoordTemplataType
  }

  test("Can explicitly coerce from kind to coord 2") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("__ParamRune_0"),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST("__ParamRune_0")),TemplexSR(NameST("Int")))),
          List(),
          None)

    conclusions.typeByRune("__ParamRune_0") shouldEqual CoordTemplataType
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(TemplexSR(RuneST("__RetRune")),CallSR("toRef",List(TemplexSR(NameST("MutStruct")))))),
          List(),
          None)

    conclusions.typeByRune("__RetRune") shouldEqual CoordTemplataType
  }

  test("Can infer from simple rules") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("__ParamRune_0"),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST("__ParamRune_0")),CallSR("toRef", List(TemplexSR(NameST("Int")))))),
          List(),
          None)

    vassert(conclusions.typeByRune("__ParamRune_0") == CoordTemplataType)
  }

  test("Can infer type from interface template param") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(
              TypedSR(Some("K"), KindTypeSR),
              TemplexSR(CallST(NameST("MutTInterface"),List(RuneST("T")))))),
          List(),
          None)

    vassert(conclusions.typeByRune("T") == CoordTemplataType)
    vassert(conclusions.typeByRune("K") == KindTemplataType)
  }

  test("Can infer templata from CallST") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(
              TypedSR(Some("X"),KindTypeSR),
              TemplexSR(CallST(NameST("MutTInterface"),List(RuneST("T")))))),
          List(),
          None)

    vassert(conclusions.typeByRune("T") == CoordTemplataType)
  }

  test("Can conjure an owning coord from a borrow coord") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("T"),CoordTypeSR),
            TypedSR(Some("__ParamRune_0K"),KindTypeSR),
            ComponentsSR(TypedSR(Some("T"),CoordTypeSR),List(TemplexSR(OwnershipST(OwnP)), TemplexSR(RuneST("__ParamRune_0K")))),
            TypedSR(Some("__ParamRune_0"),CoordTypeSR),
            ComponentsSR(TypedSR(Some("__ParamRune_0"),CoordTypeSR),List(TemplexSR(OwnershipST(BorrowP)), TemplexSR(RuneST("__ParamRune_0K"))))),
          List(AtomSP(Some(CaptureP("m",FinalP)),None,"__ParamRune_0",None)),
          None)

    conclusions.typeByRune("T") shouldEqual CoordTemplataType
  }

  test("Rune 0 upcasts to right type, simple") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("__Let0_"),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST("__Let0_")),CallSR("toRef", List(TemplexSR(NameST("MutInterface")))))),
          List(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0_",None)),
          None)

    vassert(conclusions.typeByRune("__Let0_") == CoordTemplataType)
  }

  test("Rune 0 upcasts to right type templated") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(Some("__Let0_"),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST("__Let0_")),CallSR("toRef", List(TemplexSR(CallST(NameST("MutTInterface"), List(RuneST("T")))))))),
          List(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0_",None)),
          None)

    vassert(conclusions.typeByRune("__Let0_") == CoordTemplataType)
    vassert(conclusions.typeByRune("T") == CoordTemplataType)
  }

  test("Tests destructor") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(TypedSR(Some("T"),CoordTypeSR),List(OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))), CallSR("passThroughIfConcrete",List(TemplexSR(AnonymousRuneST()))))),
        EqualsSR(TypedSR(Some("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST("Void"))))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    // Test that it does match a pack
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    vassert(conclusions.typeByRune("T") == CoordTemplataType)
  }

  test("Tests passThroughIfInterface") {
    // Tests that we can make a rule that will only match interfaces.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfInterface",List(TemplexSR(AnonymousRuneST()))))),
        EqualsSR(TypedSR(Some("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST("Void"))))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    // Test that it does match an interface
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    vassert(conclusions.typeByRune("T") == CoordTemplataType)
  }


  test("Tests passThroughIfStruct") {
    // Tests that we can make a rule that will only match structs.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfStruct",List(TemplexSR(AnonymousRuneST()))))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (conclusions, RuleTyperSolveSuccess(_)) = makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms,None)
    vassert(conclusions.typeByRune("T") == CoordTemplataType)
  }

  test("Test coercing template call result") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TypedSR(Some("__ParamRune_0"),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST("__ParamRune_0")),
          TemplexSR(CallST(NameST("MutTStruct"),List(NameST("Int"))))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)

    conclusions.typeByRune("__ParamRune_0") shouldEqual CoordTemplataType
  }

  test("Test ownershipped") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TypedSR(Some("__ParamRune_0"),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST("__ParamRune_0")),
          TemplexSR(CallST(NameST("MutTStruct"),List(OwnershippedST(ShareP,NameST("Int")))))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    conclusions.typeByRune("__ParamRune_0") shouldEqual CoordTemplataType
  }



  test("Test result of a CallAT can coerce to coord") {
    val rules =
      List(
        TypedSR(Some("__Par0"),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST("__Par0")),TemplexSR(NameST("MutStruct"))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    conclusions.typeByRune("__Par0") shouldEqual CoordTemplataType
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    val rules =
      List(
        TypedSR(Some("__ParamRune_0"),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST("__ParamRune_0")),TemplexSR(CallST(NameST("MutTStruct"),List(RuneST("T"))))))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        rules,
        List(AtomSP(Some(CaptureP("x",FinalP)),Some(AbstractSP),"__ParamRune_0",None)),
        None)
    conclusions.typeByRune("__ParamRune_0") shouldEqual CoordTemplataType
  }

  test("Test destructuring") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          TypedSR(Some("__Let0_"),CoordTypeSR),
          TypedSR(Some("__Let0__Mem_0"),CoordTypeSR),
          TypedSR(Some("__Let0__Mem_1"),CoordTypeSR)),
        List(
          AtomSP(
            None,
            None,"__Let0_",
            Some(
              List(
                Some(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0__Mem_0",None)),
                Some(AtomSP(Some(CaptureP("y",FinalP)),None,"__Let0__Mem_1",None)))))),
        Some(Set("__Let0__Mem_0", "__Let0__Mem_1", "__Let0_")))
    conclusions.typeByRune("__Let0_") shouldEqual CoordTemplataType
    conclusions.typeByRune("__Let0__Mem_0") shouldEqual CoordTemplataType
    conclusions.typeByRune("__Let0__Mem_1") shouldEqual CoordTemplataType
  }

  test("Test array sequence") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          TypedSR(Some("__ParamRune_0"),CoordTypeSR),
          EqualsSR(
            TemplexSR(RuneST("__ParamRune_0")),
            TemplexSR(RepeaterSequenceST(MutabilityST(MutableP), IntST(5),OwnershippedST(ShareP,NameST("Int")))))),
        List(),
        None)
    conclusions.typeByRune("__ParamRune_0") shouldEqual CoordTemplataType
  }

  test("Test array") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          EqualsSR(
            TypedSR(Some("K"), KindTypeSR),
            TemplexSR(CallST(NameST("__Array"),List(MutabilityST(MutableP), NameST("Int"))))),
          EqualsSR(
            TypedSR(Some("K"), KindTypeSR),
            TemplexSR(CallST(NameST("__Array"),List(RuneST("M"), RuneST("T")))))),
        List(),
        None)
    conclusions.typeByRune("M") shouldEqual MutabilityTemplataType
    conclusions.typeByRune("T") shouldEqual CoordTemplataType
  }

  test("Test evaluating isa") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            IsaSR(
              TemplexSR(RuneST("K")),
              TemplexSR(NameST("MutInterface")))),
          List(),
          None)
    conclusions.typeByRune("K") shouldEqual KindTemplataType
  }
}
