package net.verdagon.radonc.scout

import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.radonc.scout.predictor.Conclusions
import net.verdagon.radonc.scout.rules.{EqualsSR, _}
import net.verdagon.radonc.scout.templatepredictor.PredictorEvaluator
import net.verdagon.radonc.{vassert, vassertSome, vfail, vimpl}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

class RunePredictorTests extends FunSuite with Matchers {
  test("Predict doesnt crash for simple templex") {
    val conclusions =
      PredictorEvaluator.solve(
        List(TemplexSR(NameST("Int"))),
        List())
    conclusions shouldEqual Conclusions(Set(), Map())
  }

  test("Can know rune from simple equals") {
    val conclusions =
      PredictorEvaluator.solve(
        List(
          EqualsSR(TemplexSR(RuneST("T")), TemplexSR(NameST("Int")))),
        List())
    conclusions shouldEqual Conclusions(Set("T"), Map())
  }

  test("Predict for simple equals 2") {
    val conclusions =
      PredictorEvaluator.solve(
        List(
          TypedSR(Some("__ParamRune_0"),CoordTypeSR),
          EqualsSR(
            TemplexSR(RuneST("__ParamRune_0")),
            TemplexSR(CallST(NameST("MyOption"),List(OwnershippedST(ShareP, NameST("Int"))))))),
        List())
    conclusions shouldEqual Conclusions(Set("__ParamRune_0"), Map("__ParamRune_0" -> CoordTypeSR))
  }

  test("Predict doesn't know value from Or rule") {
    val conclusions =
      PredictorEvaluator.solve(
        List(
          ComponentsSR(
            TypedSR(Some("T"),CoordTypeSR),
            List(
              OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
              // Not exactly valid but itll do for this test
              OrSR(List(TypedSR(None,KindTypeSR), TypedSR(None,CoordTypeSR)))))),
        List())
    conclusions shouldEqual Conclusions(Set(), Map("T" -> CoordTypeSR))
  }

  test("Predict doesnt know T from components with anonymous kind") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfConcrete",List(TemplexSR(AnonymousRuneST()))))),
        EqualsSR(TypedSR(Some("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST("Void")))))),
      List())
    conclusions shouldEqual Conclusions(Set("V"), Map("T" -> CoordTypeSR, "V" -> CoordTypeSR))
  }

  test("Predict returns true for array sequence") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        TypedSR(Some("__ParamRune_0"),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST("__ParamRune_0")),
          TemplexSR(RepeaterSequenceST(MutabilityST(MutableP), IntST(5),OwnershippedST(ShareP,NameST("Int")))))),
      List())
    conclusions shouldEqual Conclusions(Set("__ParamRune_0"), Map("__ParamRune_0" -> CoordTypeSR))
  }

  test("Predict for idestructor for interface") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfInterface",List(TemplexSR(AnonymousRuneST()))))),
        EqualsSR(TypedSR(Some("V"),CoordTypeSR),CallSR("toRef",List(TemplexSR(NameST("Void")))))),
      List())
    conclusions shouldEqual Conclusions(Set("V"), Map("T" -> CoordTypeSR, "V" -> CoordTypeSR))

  }

  test("Predict for idestructor for struct") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.
    val conclusions =
    PredictorEvaluator.solve(
      List(
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            OrSR(List(TemplexSR(OwnershipST(OwnP)), TemplexSR(OwnershipST(ShareP)))),
            CallSR("passThroughIfStruct",List(TemplexSR(AnonymousRuneST()))))),
        CallSR("passThroughIfInterface",List(TemplexSR(RuneST("I"))))),
      List())
    conclusions shouldEqual Conclusions(Set(), Map("T" -> CoordTypeSR))
  }
}
