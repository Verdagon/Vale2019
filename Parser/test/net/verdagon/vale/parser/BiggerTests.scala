package net.verdagon.vale.parser

import org.scalatest.{FunSuite, Matchers}


trait Collector {
  def recursiveCollectFirst[T, R](a: Any, partialFunction: PartialFunction[Any, R]): Option[R] = {
    if (partialFunction.isDefinedAt(a)) {
      return Some(partialFunction.apply(a))
    }
    a match {
      case p : Product => {
        val opt: Option[R] = None
        p.productIterator.foldLeft(opt)({
          case (Some(x), _) => Some(x)
          case (None, next) => recursiveCollectFirst(next, partialFunction)
        })
      }
      case _ => None
    }
  }

  implicit class ProgramWithExpect(program: Any) {
    def shouldHave[T](f: PartialFunction[Any, T]): T = {
      recursiveCollectFirst(program, f) match {
        case None => throw new AssertionError("Couldn't find the thing, in:\n" + program)
        case Some(t) => t
      }
    }
  }
}

class BiggerTests extends FunSuite with Matchers with Collector {
  private def compile[T](parser: VParser.Parser[T], code: String): T = {
    // The strip is in here because things inside the parser don't expect whitespace before and after
    VParser.parse(parser, code.strip().toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail("Couldn't parse!\n" + input.pos.longString);
      }
      case VParser.Success(expr, rest) => {
        expr
      }
    }
  }

  test("Simple while loop") {
    compile(VParser.statement,"while () {}") shouldHave {
      case WhilePE(BlockPE(_, List(VoidPE(_))), BlockPE(_, List(VoidPE(_)))) =>
    }
  }

  test("Result after while loop") {
    compile(VParser.blockExprs,"while () {} = false;") shouldHave {
      case List(
      WhilePE(BlockPE(_, List(VoidPE(_))), BlockPE(_, List(VoidPE(_)))),
      BoolLiteralPE(_, false)) =>
    }
  }

  test("Block with result") {
    compile(VParser.blockExprs,"= 3;") shouldHave {
      case List(IntLiteralPE(_, 3)) =>
    }
  }

  test("Simple function") {
    compile(VParser.topLevelFunction, "fn sum(){3}") match {
      case FunctionP(_, Some(StringP(_, "sum")), None, None, None, None, Some(ParamsP(_,List())), None, Some(BlockPE(_, List(IntLiteralPE(_, 3))))) =>
    }
  }

//  test("Simple function with typed identifying rune") {
//    val func = compile(VParser.topLevelFunction, "fn sum<A>(a A){a}")
//    func.templateRules shouldHave {
  // case  }

  test("Function call") {
    val program = compile(VParser.program, "fn main(){call(sum)}")
//    val main = program.lookupFunction("main")

    program shouldHave {
      case FunctionCallPE(_, LookupPE(StringP(_, "call"), List()),List(LookupPE(StringP(_, "sum"), List())),true) =>
    }
  }

  test("Mutating as statement") {
    val program = compile(VParser.topLevelFunction, "fn main() { mut x = 6; }")
    program shouldHave {
      case MutatePE(_,LookupPE(StringP(_, "x"),List()),IntLiteralPE(_, 6)) =>
    }
  }





  test("Test templated lambda param") {
    val program = compile(VParser.program, "fn main(){(a){ a + a}(3)}")
    program shouldHave { case FunctionCallPE(_, LambdaPE(_), List(IntLiteralPE(_, 3)),true) => }
    program shouldHave {
      case PatternPP(_, Some(CaptureP(_,StringP(_, "a"),FinalP)),None,None,None) =>
    }
    program shouldHave {
      case FunctionCallPE(_, LookupPE(StringP(_, "+"), List()),List(LookupPE(StringP(_, "a"), List()), LookupPE(StringP(_, "a"), List())),true) =>
    }
  }

  test("Simple struct") {
    compile(VParser.struct, "struct Moo { x &Int; }") shouldHave {
      case StructP(_, StringP(_, "Moo"), false, MutableP, None, None, List(StructMemberP(StringP(_, "x"), FinalP, BorrowPT(NameOrRunePT(StringP(_, "Int")))))) =>
    }
  }

  test("Export struct") {
    compile(VParser.struct, "struct Moo export { x &Int; }") shouldHave {
      case StructP(_, StringP(_, "Moo"), true, MutableP, None, None, List(StructMemberP(StringP(_, "x"), FinalP, BorrowPT(NameOrRunePT(StringP(_, "Int")))))) =>
    }
  }

  test("Test block's trailing void presence") {
    compile(VParser.filledBody, "{ moo() }") shouldHave {
      case BlockPE(_, List(FunctionCallPE(_, LookupPE(StringP(_, "moo"), List()), List(), true))) =>
    }

    compile(VParser.filledBody, "{ moo(); }") shouldHave {
      case BlockPE(_, List(FunctionCallPE(_, LookupPE(StringP(_, "moo"), List()), List(), true), VoidPE(_))) =>
    }
  }

  test("ifs") {
    compile(VParser.ifLadder, "if (true) { doBlarks(&x) } else { }") shouldHave {
      case IfPE(_,
      BlockPE(_, List(BoolLiteralPE(_, true))),
      BlockPE(_, List(FunctionCallPE(_, LookupPE(StringP(_, "doBlarks"), List()), List(LendPE(LookupPE(StringP(_, "x"), List()))), true))),
      BlockPE(_, List(VoidPE(_)))) =>
    }
  }

  test("Block with only a result") {
    compile(
      VParser.blockExprs,
      "= doThings(a);") shouldHave {
      case List(FunctionCallPE(_, LookupPE(StringP(_, "doThings"), List()), List(LookupPE(StringP(_, "a"), List())), true)) =>
    }
  }


  test("Block with statement and result") {
    compile(
      VParser.blockExprs,
      """
        |b;
        |= a;
      """.stripMargin) shouldHave {
      case List(LookupPE(StringP(_, "b"), List()), LookupPE(StringP(_, "a"), List())) =>
    }
  }


  test("Block with result that could be an expr") {
    // = doThings(a); could be misinterpreted as an expression doThings(=, a) if we're
    // not careful.
    compile(
      VParser.blockExprs,
      """
        |a = 2;
        |= doThings(a);
      """.stripMargin) shouldHave {
      case List(
          LetPE(_,List(), PatternPP(_, Some(CaptureP(_,StringP(_, "a"), FinalP)), None, None, None), IntLiteralPE(_, 2)),
            FunctionCallPE(_, LookupPE(StringP(_, "doThings"), List()), List(LookupPE(StringP(_, "a"), List())), true)) =>
    }
  }

  test("Templated impl") {
    compile(
      VParser.impl,
      """
        |impl<T> SomeStruct<T> for MyInterface<T>;
      """.stripMargin) shouldHave {
      case ImplP(_,
      Some(IdentifyingRunesP(_, List(StringP(_, "T")))),
      None,
      CallPPT(NameOrRunePPT(StringP(_, "SomeStruct")), List(NameOrRunePPT(StringP(_, "T")))),
      CallPPT(NameOrRunePPT(StringP(_, "MyInterface")), List(NameOrRunePPT(StringP(_, "T"))))) =>
    }
  }

  test("Impling a template call") {
    compile(
      VParser.impl,
      """
        |impl MyIntIdentity for IFunction1<mut, Int, Int>;
        |""".stripMargin) shouldHave {
      case ImplP(_,
      None,
      None,
      NameOrRunePPT(StringP(_, "MyIntIdentity")),
      CallPPT(NameOrRunePPT(StringP(_, "IFunction1")), List(MutabilityPPT(MutableP), NameOrRunePPT(StringP(_, "Int")), NameOrRunePPT(StringP(_, "Int"))))) =>
    }
  }


  test("Virtual function") {
    compile(
      VParser.topLevelFunction,
      """
        |fn doCivicDance(virtual this Car) Int;
      """.stripMargin) shouldHave {
      case FunctionP(
        _,
        Some(StringP(_, "doCivicDance")), None, None, None, None,
        Some(ParamsP(_, List(PatternPP(_, Some(CaptureP(_,StringP(_, "this"), FinalP)), Some(NameOrRunePPT(StringP(_, "Car"))), None, Some(AbstractP))))),
        Some(NameOrRunePPT(StringP(_, "Int"))), None) =>
    }
  }


  test("17") {
    compile(
      VParser.structMember,
      "a *ListNode<T>;") shouldHave {
      case StructMemberP(StringP(_, "a"), FinalP, SharePT(CallPT(NameOrRunePT(StringP(_, "ListNode")), List(NameOrRunePT(StringP(_, "T")))))) =>
    }
  }

  test("18") {
    compile(
      VParser.structMember,
      "a Array<imm, T>;") shouldHave {
      case StructMemberP(StringP(_, "a"), FinalP, CallPT(NameOrRunePT(StringP(_, "Array")), List(MutabilityPT(ImmutableP), NameOrRunePT(StringP(_, "T"))))) =>
    }
  }

  test("19") {
    compile(VParser.statement,
      "newLen = if (num == 0) { 1 } else { 2 };") shouldHave {
      case LetPE(_,
      List(),
      PatternPP(_, Some(CaptureP(_,StringP(_, "newLen"), FinalP)), None, None, None),
      IfPE(_,
      BlockPE(_, List(FunctionCallPE(_, LookupPE(StringP(_, "=="), List()), List(LookupPE(StringP(_, "num"), List()), IntLiteralPE(_, 0)), true))),
      BlockPE(_, List(IntLiteralPE(_, 1))),
      BlockPE(_, List(IntLiteralPE(_, 2))))) =>
    }
  }

  test("20") {
    compile(VParser.expression,
      "weapon.owner.map()") shouldHave {
      case MethodCallPE(_,
        DotPE(_,
          LookupPE(StringP(_,"weapon"),List()),
          LookupPE(StringP(_,"owner"),List())),
        true,
        LookupPE(StringP(_,"map"),List()),
        List()) =>
    }
  }

  test("!=") {
    compile(VParser.expression,"3 != 4") shouldHave {
      case FunctionCallPE(_, LookupPE(StringP(_, "!="), List()), List(IntLiteralPE(_, 3), IntLiteralPE(_, 4)), true) =>
    }
  }
}
