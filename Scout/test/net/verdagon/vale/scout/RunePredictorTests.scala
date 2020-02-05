package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules.{EqualsSR, _}
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class RunePredictorTests extends FunSuite with Matchers {
  test("Predict doesnt crash for simple templex") {
    val conclusions =
      PredictorEvaluator.solve(List(TemplexSR(NameST(CodeTypeNameS("Int")))), List())
    conclusions shouldEqual Conclusions(Set(), Map())
  }

  test("Can know rune from simple equals") {
    val conclusions =
      PredictorEvaluator.solve(
        List(
          EqualsSR(TemplexSR(RuneST(CodeRuneS("T"))), TemplexSR(NameST(CodeTypeNameS("Int"))))),
        List())
    conclusions shouldEqual Conclusions(Set(CodeRuneS("T")), Map())
  }

  test("Predict for simple equals 2") {
    val conclusions =
      PredictorEvaluator.solve(
        List(
          TypedSR(ImplicitRuneS(0),CoordTypeSR),
          EqualsSR(
            TemplexSR(RuneST(ImplicitRuneS(0))),
            TemplexSR(CallST(NameST(CodeTypeNameS("MyOption")),List(OwnershippedST(ShareP, NameST(CodeTypeNameS("Int")))))))),
        List())
    conclusions shouldEqual Conclusions(Set(ImplicitRuneS(0)), Map(ImplicitRuneS(0) -> CoordTypeSR))
  }

  test("Predict doesn't know value from Or rule") {
    val tRune = CodeRuneS("T")
    val aRune = ImplicitRuneS(0)
    val bRune = ImplicitRuneS(1)
    val conclusions =
      PredictorEvaluator.solve(
        List(
          ComponentsSR(
            TypedSR(tRune,CoordTypeSR),
            List(
              OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
              // Not exactly valid but itll do for this test
              OrSR(List(TypedSR(aRune,KindTypeSR), TypedSR(bRune,CoordTypeSR)))))),
        List())
    conclusions shouldEqual
      Conclusions(Set(), Map(tRune -> CoordTypeSR, aRune -> KindTypeSR, bRune -> CoordTypeSR))
  }

  test("Predict doesnt know T from components with anonymous kind") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(CodeRuneS("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfConcrete",List(TemplexSR(RuneST(ImplicitRuneS(0))))))),
        EqualsSR(TypedSR(CodeRuneS("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST(CodeTypeNameS("Void"))))))),
      List())
    conclusions shouldEqual
      Conclusions(
        Set(CodeRuneS("V")),
        Map(
          CodeRuneS("T") -> CoordTypeSR,
          CodeRuneS("V") -> CoordTypeSR))
  }

  test("Predict returns true for array sequence") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        TypedSR(ImplicitRuneS(0),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST(ImplicitRuneS(0))),
          TemplexSR(RepeaterSequenceST(MutabilityST(MutableP), IntST(5),OwnershippedST(ShareP,NameST(CodeTypeNameS("Int"))))))),
      List())
    conclusions shouldEqual Conclusions(Set(ImplicitRuneS(0)), Map(ImplicitRuneS(0) -> CoordTypeSR))
  }

  test("Predict for idestructor for interface") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(CodeRuneS("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfInterface",List(TemplexSR(RuneST(ImplicitRuneS(0))))))),
        EqualsSR(TypedSR(CodeRuneS("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST(CodeTypeNameS("Void"))))))),
      List())
    conclusions shouldEqual
      Conclusions(
        Set(CodeRuneS("V")),
        Map(
          CodeRuneS("T") -> CoordTypeSR,
          CodeRuneS("V") -> CoordTypeSR))

  }

  test("Predict for idestructor for struct") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(CodeRuneS("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfStruct",List(TemplexSR(RuneST(ImplicitRuneS(0))))))),
        CallSR("passThroughIfInterface",List(TemplexSR(RuneST(CodeRuneS("I")))))),
      List())
    conclusions shouldEqual Conclusions(Set(), Map(CodeRuneS("T") -> CoordTypeSR))
  }
}
