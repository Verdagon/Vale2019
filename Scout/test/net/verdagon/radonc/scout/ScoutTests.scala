package net.verdagon.radonc.scout

import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class ScoutTests extends FunSuite with Matchers {
  private def compile(code: String): ProgramS = {
    VParser.parse(VParser.program, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(program0, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        Scout.scoutProgram(program0)
      }
    }
  }

  test("Lookup +") {
    val program1 = compile("fn main() { +(3, 4) }")
    val main = program1.lookupFunction("main")
    
    val CodeBody1(BodySE(_, block)) = main.body
    block match {
      case BlockSE(_, List(FunctionCallSE(GlobalLoadSE("+"), _))) =>
    }
  }

  test("Struct") {
    val program1 = compile("struct Moo { x: Int; }")
    val imoo = program1.lookupStruct("Moo")

    imoo.rules shouldEqual List(EqualsSR(TypedSR(Some("Mem_0"),CoordTypeSR),TemplexSR(NameST("Int"))))
    imoo.members shouldEqual List(StructMemberS("x",FinalP,"Mem_0"))
  }

  test("Lambda") {
    val program1 = compile("fn main() { {_ + _}(4, 6) }")

    val CodeBody1(BodySE(_, BlockSE(_, List(expr)))) = program1.lookupFunction("main").body
    val FunctionCallSE(FunctionSE(lambda @ FunctionS(_, _, _, _, _, _, _, _,_, _, _, _, _)), _) = expr
    lambda.identifyingRunes shouldEqual
      List(Scout.unrunedParamRunePrefix + "0", Scout.unrunedParamRunePrefix + "1")
  }

  test("Interface") {
    val program1 = compile("interface IMoo { fn blork(a: Bool)Void; }")
    val imoo = program1.lookupInterface("IMoo")

    imoo.rules shouldEqual List()

    val expectedRulesS =
      List(
        TypedSR(Some("__Par0"),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST("__Par0")),TemplexSR(NameST("Bool"))),
        TypedSR(Some("__Ret"),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST("__Ret")),TemplexSR(NameST("Void"))))

    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual List("__Par0", "__Ret")
    program1.lookupFunction("blork").templateRules shouldEqual expectedRulesS

    program1.lookupFunction("blork").params shouldEqual
      List(ParameterS(AtomSP(Some(CaptureP("a",FinalP)),None,"__Par0",None)))

    // Yes, even though the user didnt specify any. See CCAUIR.
    program1.lookupFunction("blork").identifyingRunes shouldEqual List()
  }

  test("Impl") {
    val program1 = compile("impl Moo for IMoo;")
    val impl = program1.impls.head
    impl.structKindRune shouldEqual "__0"
    impl.interfaceKindRune shouldEqual "__1"
    impl.rules shouldEqual
      List(
        TypedSR(Some("__0"),KindTypeSR),
        EqualsSR(TemplexSR(RuneST("__0")), TemplexSR(NameST("Moo"))),
        TypedSR(Some("__1"),KindTypeSR),
        EqualsSR(TemplexSR(RuneST("__1")), TemplexSR(NameST("IMoo"))))
  }

  test("Method call") {
    val program1 = compile("fn main() { x = 4; = x.shout(); }")
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    block match {
      case BlockSE(_, List(_, FunctionCallSE(GlobalLoadSE("shout"), PackSE(List(ExpressionLendSE(LocalLoadSE("x", true))))))) =>
    }
  }

  test("Moving method call") {
    val program1 = compile("fn main() { x = 4; = x^.shout(); }")
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    block match {
      case BlockSE(_, List(_, FunctionCallSE(GlobalLoadSE("shout"), PackSE(List(LocalLoadSE("x", false)))))) =>
    }
  }

  test("Function with magic lambda and regular lambda") {
    // There was a bug that confused the two, and an underscore would add a magic param to every lambda after it

    val program1 =
      compile(
        """fn main() {
          |  {_};
          |  {(a) a};
          |}
        """.stripMargin)
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    val BlockSE(_, FunctionSE(lambda1) :: FunctionSE(lambda2) :: _) = block
    val List(_, ParameterS(AtomSP(Some(CaptureP("__param_0",FinalP)),None,_,None))) = lambda1.params
    val List(_, ParameterS(AtomSP(Some(CaptureP("a",FinalP)),None,_,None))) = lambda2.params
  }


  def runTests() {
//    check(1,
//        "fn main(){3}",
//        ProgramS(List(), List(), List(
//            FunctionS("main",0,false,false,true, List(),List(),None,
//                Some(BodySE(Set(),Set(),List(IntLiteral1(3))))))));
//    check(3,
//        "fn main(){4 - 3}",
//        ProgramS(List(), List(), List(
//            FunctionS("main",0,false,false,true,List(),List(),None,
//              Some(BodySE(Set(),Set(),List(Scramble1(List(IntLiteral1(4), LookuSP("-"), IntLiteral1(3))))))))));
//    check(4,
//        "fn main(){{(a: Int) +(a,a)}(3)}",
//        ProgramS(List(), List(), List(
//            FunctionS("main",0,false,false,true,List(),List(),None,
//              Some(BodySE(Set(),Set(),List(
//                Scramble1(List(
//                    FunctionS("main:lam1",1,false,false,true,List(),List(ParameterS(None,2, "a", CaptureSP("a", FinalP,Some(TypeOfSP(TypeName1("Int")))))),None,
//                      Some(BodySE(Set(),Set(),List(
//                        Scramble1(List(LookuSP("+"), scout.PackSE(List(LookuSP("a"), LookuSP("a"))))))))),
//                  scout.PackSE(List(IntLiteral1(3))))))))))));
//    check(5,"fn main(){{(a) a - 2}(3)}",
//      ProgramS(List(), List(), List(
//        FunctionS("main",0,false,false,true,List(),List(),None,
//          Some(BodySE(Set(),Set(),List(Scramble1(List(
//            FunctionS("main:lam1",1,false,false,true,List(),List(ParameterS(None,3,"a", CaptureSP("a", FinalP,Some(TypeOfSP(TypeName1("__T2")))))),None,
//              Some(BodySE(Set(),Set(),List(Scramble1(List(LookuSP("a"), LookuSP("-"), IntLiteral1(2))))))),
//            PackSE(List(IntLiteral1(3))))))))))))
//    check(6,"fn main(){{(_) 2}(3)}",
//      ProgramS(List(), List(), List(FunctionS("main",0,false,false,true,List(),List(),None,Some(BodySE(Set(),Set(),List(Scramble1(List(
//        FunctionS("main:lam1",1,false,false,true,List(TemplateParameter1("__T2", ReferenceTemplataType1)),List(ParameterS(None,3,"__P2", CaptureSP("__P2", FinalP,Some(TypeOfSP(TypeName1("__T2")))))),None,
//          Some(BodySE(Set(),Set(),List(IntLiteral1(2))))),
//        PackSE(List(IntLiteral1(3))))))))))));
//    check(7,
//        "fn main(){let $a = 3; mut a = 4;}",
//        ProgramS(List(), List(), List(
//            FunctionS("main",0,false,false,true,List(),List(),None,
//              Some(BodySE(Set(),Set(),List(
//                    Let1(1, "a", CaptureSP("a",true,None),IntLiteral1(3)),
//                    LocalMutate1("a",IntLiteral1(4)),
//                  scout.PackSE(List()))))))));
//
//    // Make sure it's in the lambda's capture list but not main's
//    check(8,
//        "fn main() { x = 4; { print x }() }",
//        ProgramS(List(), List(), List(
//          FunctionS("main",0,false,false,true,List(),List(),None,
//            Some(BodySE(Set(),Set(),List(
//              Let1(1,"x", CaptureSP("x", FinalP,None),IntLiteral1(4)),
//              Scramble1(List(
//                FunctionS("main<lam2>",2,false,false,true,List(),List(),None,
//                  Some(BodySE(Set(),Set(),List(Scramble1(List(LookuSP("print"), LookuSP("x"))))))),
//                scout.PackSE(List()))))))))))
//
//
//    // Make sure the outer lambda only captures x, and main captures nothing
//    check(9,
//        "fn main() { x = 4; { y = 5; { x + y }() }() }",
//        ProgramS(List(), List(), List(
//          FunctionS("main",0,false,false,true,List(),List(),None,Some(BodySE(Set(),Set(),List(
//            Let1(1,"x", CaptureSP("x", FinalP,None),IntLiteral1(4)),
//            Scramble1(List(
//              FunctionS("main<lam2>",1,false,false,true,List(),List(),None,Some(BodySE(Set(),Set(),List(
//                Let1(3,"y", CaptureSP("y", FinalP,None),IntLiteral1(5)),
//                Scramble1(List(
//                  FunctionS("main<lam4>",2,false,false,true,List(),List(),None,
//                    Some(BodySE(Set(),Set(),List(Scramble1(List(LookuSP("x"), LookuSP("+"), LookuSP("y"))))))),
//                  scout.PackSE(List()))))))),
//              scout.PackSE(List()))))))))))
  }
}
