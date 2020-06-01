package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.{vassert, vfail}
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
      case BlockSE(_, List(FunctionCallSE(FunctionLoadSE(GlobalFunctionFamilyNameS("+")), _))) =>
    }
  }

  test("Struct") {
    val program1 = compile("struct Moo { x Int; }")
    val imoo = program1.lookupStruct("Moo")

    val mooName =
        TopLevelCitizenDeclarationNameS("Moo",CodeLocationS(1,1))

    val memberRune = MemberRuneS(0)
    imoo.rules match {
      case List(
        EqualsSR(TypedSR(memberRune, CoordTypeSR), TemplexSR(NameST(CodeTypeNameS("Int")))),
        EqualsSR(TemplexSR(RuneST(ImplicitRuneS(_, _))), TemplexSR(MutabilityST(MutableP)))) =>
    }
    imoo.members shouldEqual List(StructMemberS("x",FinalP,memberRune))
  }

  test("Lambda") {
    val program1 = compile("fn main() { {_ + _}(4, 6) }")

    val CodeBody1(BodySE(_, BlockSE(_, List(expr)))) = program1.lookupFunction("main").body
    val FunctionCallSE(FunctionSE(lambda @ FunctionS(_, _, _, _, _, _,_, _, _, _)), _) = expr
    lambda.identifyingRunes match {
      case List(MagicParamRuneS(mp1), MagicParamRuneS(mp2)) => {
        vassert(mp1 != mp2)
      }
    }
  }

  test("Interface") {
    val program1 = compile("interface IMoo { fn blork(a Bool)Void; }")
    val imoo = program1.lookupInterface("IMoo")

    imoo.rules match {
      case List(EqualsSR(TemplexSR(RuneST(ImplicitRuneS(_, _))), TemplexSR(MutabilityST(MutableP)))) =>
    }

    val blork = imoo.internalMethods.head
    blork.name match { case FunctionNameS("blork", _) => }

    val (paramRune, retRune) =
      blork.templateRules match {
        case List(
          EqualsSR(
            TypedSR(actualParamRune, CoordTypeSR),
            TemplexSR(NameST(CodeTypeNameS("Bool")))),
          EqualsSR(
            TypedSR(actualRetRune, CoordTypeSR),
            TemplexSR(NameST(CodeTypeNameS("Void"))))) => {
          actualParamRune match {
            case ImplicitRuneS(_, 0) =>
          }
          actualRetRune match {
            case ImplicitRuneS(_, 1) =>
          }
          (actualParamRune, actualRetRune)
        }
      }

    RuleSUtils.getDistinctOrderedRunesForRulexes(blork.templateRules) shouldEqual
      List(paramRune, retRune)

    blork.params match {
      case List(
        ParameterS(
          AtomSP(
            CaptureS(CodeVarNameS("a"),FinalP),
            None,
            ImplicitRuneS(_, 0),
            None))) =>
    }

    // Yes, even though the user didnt specify any. See CCAUIR.
    blork.identifyingRunes shouldEqual List()
  }

  test("Impl") {
    val program1 = compile("impl Moo for IMoo;")
    val impl = program1.impls.head
    val structRune =
      impl.structKindRune match {
        case ir0 @ ImplicitRuneS(_, 0) => ir0
      }
    val interfaceRune =
      impl.interfaceKindRune match {
        case ir0 @ ImplicitRuneS(_, 1) => ir0
      }
    impl.rules match {
      case List(
          EqualsSR(TypedSR(a,KindTypeSR), TemplexSR(NameST(CodeTypeNameS("Moo")))),
          EqualsSR(TypedSR(b,KindTypeSR), TemplexSR(NameST(CodeTypeNameS("IMoo"))))) => {
        vassert(a == structRune)
        vassert(b == interfaceRune)
      }
    }
  }

  test("Method call") {
    val program1 = compile("fn main() { x = 4; = x.shout(); }")
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    block match {
      case BlockSE(_, List(_, FunctionCallSE(FunctionLoadSE(GlobalFunctionFamilyNameS("shout")), List(ExpressionLendSE(LocalLoadSE(name, true)))))) => {
        name match {
          case CodeVarNameS("x") =>
        }
      }
    }
  }

  test("Moving method call") {
    val program1 = compile("fn main() { x = 4; = x^.shout(); }")
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    block match {
      case BlockSE(_, List(_, FunctionCallSE(FunctionLoadSE(GlobalFunctionFamilyNameS("shout")), List(LocalLoadSE(_, false))))) =>
    }
  }

  test("Function with magic lambda and regular lambda") {
    // There was a bug that confused the two, and an underscore would add a magic param to every lambda after it

    val program1 =
      compile(
        """fn main() {
          |  {_};
          |  (a){a};
          |}
        """.stripMargin)
    val main = program1.lookupFunction("main")

    val CodeBody1(BodySE(_, block)) = main.body
    val BlockSE(_, FunctionSE(lambda1) :: FunctionSE(lambda2) :: _) = block
    lambda1.params match {
      case List(_, ParameterS(AtomSP(CaptureS(MagicParamNameS(_),FinalP),None,MagicParamRuneS(_),None))) =>
    }
    lambda2.params match {
      case List(_, ParameterS(AtomSP(CaptureS(CodeVarNameS("a"),FinalP),None,ImplicitRuneS(_, _),None))) =>
    }
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
//        "fn main(){(a: Int){ +(a,a)}(3)}",
//        ProgramS(List(), List(), List(
//            FunctionS("main",0,false,false,true,List(),List(),None,
//              Some(BodySE(Set(),Set(),List(
//                Scramble1(List(
//                    FunctionS("main:lam1",1,false,false,true,List(),List(ParameterS(None,2, "a", CaptureSP("a", FinalP,Some(TypeOfSP(TypeName1("Int")))))),None,
//                      Some(BodySE(Set(),Set(),List(
//                        Scramble1(List(LookuSP("+"), scout.PackSE(List(LookuSP("a"), LookuSP("a"))))))))),
//                  scout.PackSE(List(IntLiteral1(3))))))))))));
//    check(5,"fn main(){(a){ a - 2}(3)}",
//      ProgramS(List(), List(), List(
//        FunctionS("main",0,false,false,true,List(),List(),None,
//          Some(BodySE(Set(),Set(),List(Scramble1(List(
//            FunctionS("main:lam1",1,false,false,true,List(),List(ParameterS(None,3,"a", CaptureSP("a", FinalP,Some(TypeOfSP(TypeName1("__T2")))))),None,
//              Some(BodySE(Set(),Set(),List(Scramble1(List(LookuSP("a"), LookuSP("-"), IntLiteral1(2))))))),
//            PackSE(List(IntLiteral1(3))))))))))))
//    check(6,"fn main(){(_){ 2}(3)}",
//      ProgramS(List(), List(), List(FunctionS("main",0,false,false,true,List(),List(),None,Some(BodySE(Set(),Set(),List(Scramble1(List(
//        FunctionS("main:lam1",1,false,false,true,List(TemplateParameter1("__T2", ReferenceTemplataType1)),List(ParameterS(None,3,"__P2", CaptureSP("__P2", FinalP,Some(TypeOfSP(TypeName1("__T2")))))),None,
//          Some(BodySE(Set(),Set(),List(IntLiteral1(2))))),
//        PackSE(List(IntLiteral1(3))))))))))));
//    check(7,
//        "fn main(){let $a = 3; a! = 4;}",
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
