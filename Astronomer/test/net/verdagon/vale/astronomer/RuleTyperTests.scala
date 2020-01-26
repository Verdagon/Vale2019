package net.verdagon.vale.astronomer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{EqualsSR, _}
import net.verdagon.vale._
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
  override def lookupType(state: FakeState, env: SimpleEnvironment, absoluteName: AbsoluteNameS[INameS]): ITemplataType = {
    absoluteName.last match {
      case TopLevelCitizenDeclarationNameS(name, _) => env.lookupType(name)
    }
  }
  override def lookupType(state: FakeState, env: SimpleEnvironment, impreciseName: ImpreciseNameS[CodeTypeNameS]): ITemplataType = {
    impreciseName.last match {
      case CodeTypeNameS(name) => env.lookupType(name)
    }
  }
}

class RuleTyperTests extends FunSuite with Matchers {
  def makeCannedEnvironment(): SimpleEnvironment = {
    SimpleEnvironment(
      Map(
        "ImmInterface" -> List(KindTemplataType),
        "Array" -> List(TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
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
        override def lookupType(state: FakeState, env: SimpleEnvironment, absoluteName: AbsoluteNameS[INameS]): ITemplataType = {
          absoluteName.last match {
            case TopLevelCitizenDeclarationNameS(name, _) => env.lookupType(name)
          }
        }
        override def lookupType(state: FakeState, env: SimpleEnvironment, impreciseName: ImpreciseNameS[CodeTypeNameS]): ITemplataType = {
          impreciseName.last match {
            case CodeTypeNameS(name) => env.lookupType(name)
          }
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
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("__C")),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("__C")))),TemplexSR(OwnershippedST(BorrowP,NameST(ImpreciseNameS(List(), CodeTypeNameS("ImmInterface"))))))),
          List(),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__C"))) == CoordTemplataType)
  }

  test("Can infer coord rune from an incoming kind") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(TypedSR(AbsoluteNameS("", List(), CodeRuneS("C")), CoordTypeSR)),
          List(),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("C"))) == CoordTemplataType)
  }

  test("Detects conflict between types") {
    val (_, isf @ RuleTyperSolveFailure(_, _, _)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(EqualsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("C")), CoordTypeSR), TypedSR(AbsoluteNameS("", List(), CodeRuneS("C")), KindTypeSR))),
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
          List(EqualsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("C")), CoordTypeSR), CallSR("toRef", List(TypedSR(AbsoluteNameS("", List(), CodeRuneS("A")), KindTypeSR))))),
          List(),
          None)

    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("C"))) shouldEqual CoordTemplataType
  }

  test("Can explicitly coerce from kind to coord 2") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))),
          List(),
          None)

    conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplataType
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("__RetRune")))),CallSR("toRef",List(TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutStruct")))))))),
          List(),
          None)

    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__RetRune"))) shouldEqual CoordTemplataType
  }

  test("Can infer from simple rules") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),CallSR("toRef", List(TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))))),
          List(),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) == CoordTemplataType)
  }

  test("Can infer type from interface template param") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(
              TypedSR(AbsoluteNameS("", List(), CodeRuneS("K")), KindTypeSR),
              TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTInterface"))),List(RuneST(AbsoluteNameS("", List(), CodeRuneS("T")))))))),
          List(),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("K"))) == KindTemplataType)
  }

  test("Can infer templata from CallST") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            EqualsSR(
              TypedSR(AbsoluteNameS("", List(), CodeRuneS("X")),KindTypeSR),
              TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTInterface"))),List(RuneST(AbsoluteNameS("", List(), CodeRuneS("T")))))))),
          List(),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
  }

  test("Can conjure an owning coord from a borrow coord") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("T")),CoordTypeSR),
            TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(1448)),KindTypeSR),
            ComponentsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("T")),CoordTypeSR),List(TemplexSR(OwnershipST(OwnP)), TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(1448)))))),
            TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
            ComponentsSR(TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),List(TemplexSR(OwnershipST(BorrowP)), TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(1448))))))),
          List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("m")),FinalP),None,AbsoluteNameS("", List(), ImplicitRuneS(0)),None)),
          None)

    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) shouldEqual CoordTemplataType
  }

  test("Rune 0 upcasts to right type, simple") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Let0_")),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("__Let0_")))),CallSR("toRef", List(TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutInterface")))))))),
          List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("x")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("__Let0_")),None)),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) == CoordTemplataType)
  }

  test("Rune 0 upcasts to right type templated") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Let0_")),CoordTypeSR),
            EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("__Let0_")))),CallSR("toRef", List(TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTInterface"))), List(RuneST(AbsoluteNameS("", List(), CodeRuneS("T")))))))))),
          List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("x")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("__Let0_")),None)),
          None)

    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) == CoordTemplataType)
    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
  }

  test("Tests destructor") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("T")),CoordTypeSR),List(OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))), CallSR("passThroughIfConcrete",List(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))))))),
        EqualsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("V")),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Void"))))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    // Test that it does match a pack
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
  }

  test("Tests passThroughIfInterface") {
    // Tests that we can make a rule that will only match interfaces.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(
          TypedSR(AbsoluteNameS("", List(), CodeRuneS("T")),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfInterface",List(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))))))),
        EqualsSR(TypedSR(AbsoluteNameS("", List(), CodeRuneS("V")),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Void"))))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    // Test that it does match an interface
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
  }


  test("Tests passThroughIfStruct") {
    // Tests that we can make a rule that will only match structs.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        ComponentsSR(
          TypedSR(AbsoluteNameS("", List(), CodeRuneS("T")),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfStruct",List(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    val (conclusions, RuleTyperSolveSuccess(_)) = makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms,None)
    vassert(conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplataType)
  }

  test("Test coercing template call result") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),
          TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTStruct"))),List(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int"))))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)

    conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplataType
  }

  test("Test ownershipped") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),
          TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTStruct"))),List(OwnershippedST(ShareP,NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplataType
  }



  test("Test result of a CallAT can coerce to coord") {
    val rules =
      List(
        TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Par0")),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("__Par0")))),TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutStruct"))))))
    val atoms =
      List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("this")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("T")),None))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(FakeState(), makeCannedEnvironment(), rules, atoms, None)
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Par0"))) shouldEqual CoordTemplataType
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    val rules =
      List(
        TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutTStruct"))),List(RuneST(AbsoluteNameS("", List(), CodeRuneS("T"))))))))

    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        rules,
        List(AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("x")),FinalP),Some(AbstractSP),AbsoluteNameS("", List(), ImplicitRuneS(0)),None)),
        None)
    conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplataType
  }

  test("Test destructuring") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Let0_")),CoordTypeSR),
          TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Let0__Mem_0")),CoordTypeSR),
          TypedSR(AbsoluteNameS("", List(), CodeRuneS("__Let0__Mem_1")),CoordTypeSR)),
        List(
          AtomSP(
            CaptureS(AbsoluteNameS("", List(), CodeVarNameS("x")), FinalP),
            None,
            AbsoluteNameS("", List(), CodeRuneS("__Let0_")),
            Some(
              List(
                AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("x")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("__Let0__Mem_0")),None),
                AtomSP(CaptureS(AbsoluteNameS("", List(), CodeVarNameS("y")),FinalP),None,AbsoluteNameS("", List(), CodeRuneS("__Let0__Mem_1")),None))))),
        Some(Set(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0")), AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1")), AbsoluteNameA("", List(), CodeRuneA("__Let0_")))))
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) shouldEqual CoordTemplataType
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0"))) shouldEqual CoordTemplataType
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1"))) shouldEqual CoordTemplataType
  }

  test("Test array sequence") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          TypedSR(AbsoluteNameS("", List(), ImplicitRuneS(0)),CoordTypeSR),
          EqualsSR(
            TemplexSR(RuneST(AbsoluteNameS("", List(), ImplicitRuneS(0)))),
            TemplexSR(RepeaterSequenceST(MutabilityST(MutableP), IntST(5),OwnershippedST(ShareP,NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))))),
        List(),
        None)
    conclusions.typeByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplataType
  }

  test("Test array") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper().solve(
        FakeState(),
        makeCannedEnvironment(),
        List(
          EqualsSR(
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("K")), KindTypeSR),
            TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("Array"))),List(MutabilityST(MutableP), NameST(ImpreciseNameS(List(), CodeTypeNameS("Int"))))))),
          EqualsSR(
            TypedSR(AbsoluteNameS("", List(), CodeRuneS("K")), KindTypeSR),
            TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("Array"))),List(RuneST(AbsoluteNameS("", List(), CodeRuneS("M"))), RuneST(AbsoluteNameS("", List(), CodeRuneS("T")))))))),
        List(),
        None)
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("M"))) shouldEqual MutabilityTemplataType
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) shouldEqual CoordTemplataType
  }

  test("Test evaluating isa") {
    val (conclusions, RuleTyperSolveSuccess(_)) =
      makeCannedRuleTyper()
        .solve(
          FakeState(),
          makeCannedEnvironment(),
          List(
            IsaSR(
              TemplexSR(RuneST(AbsoluteNameS("", List(), CodeRuneS("K")))),
              TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("MutInterface")))))),
          List(),
          None)
    conclusions.typeByRune(AbsoluteNameA("", List(), CodeRuneA("K"))) shouldEqual KindTemplataType
  }
}
