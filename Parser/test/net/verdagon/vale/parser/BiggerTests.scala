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

  implicit class ProgramWithExpect(program: Program0) {
    def shouldHaveMatch[T](f: PartialFunction[Any, T]): T = {
      recursiveCollectFirst(program, f) match {
        case None => throw new AssertionError("Couldn't find the thing, in:\n" + program)
        case Some(t) => t
      }
    }
    def shouldHave(needle: Any): Unit = {
      recursiveCollectFirst(program, { case x if needle == x => }) match {
        case None => throw new AssertionError("Couldn't find " + needle + ", in:\n" + program)
        case Some(_) =>
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
    compile(VParser.statement,"while () {}") shouldEqual
        WhilePE(BlockPE(List(VoidPE())), BlockPE(List(VoidPE())))
  }

  test("Result after while loop") {
    compile(VParser.block,"while () {} = false;") shouldEqual
        BlockPE(List(
          WhilePE(BlockPE(List(VoidPE())), BlockPE(List(VoidPE()))),
          BoolLiteralPE(false)))
  }

  test("Block with result") {
    compile(VParser.block,"= 3;") shouldEqual BlockPE(List(IntLiteralPE(3)))
  }

  test("Simple function") {
    compile(VParser.topLevelFunction, "fn sum(){3}") shouldEqual
      FunctionP(Some("sum"),false,false,true,List(),List(), List(),None,Some(BlockPE(List(IntLiteralPE(3)))))
  }

//  test("Simple function with typed identifying rune") {
//    val func = compile(VParser.topLevelFunction, "fn sum<A>(a A){a}")
//    func.templateRules shouldEqual
//  }

  test("Function call") {
    val program = compile(VParser.program, "fn main(){call(sum)}")
    program shouldHave FunctionCallPE(LookupPE("call", List()),List(LookupPE("sum", List())),true)
  }

  test("Mutating as statement") {
    val program = compile(VParser.topLevelFunction, "fn main() { mut x = 6; }")
    program.body.get.elements.head shouldEqual MutatePE(LookupPE("x",List()),IntLiteralPE(6))
  }





  test("Test templated lambda param") {
    val program = compile(VParser.program, "fn main(){(a){ a + a}(3)}")
    program shouldHaveMatch { case FunctionCallPE(LambdaPE(_), List(IntLiteralPE(3)),true) => }
    program shouldHave PatternPP(Some(CaptureP("a",FinalP)),None,None,None)
    program shouldHave FunctionCallPE(LookupPE("+", List()),List(LookupPE("a", List()), LookupPE("a", List())),true)
  }

  test("Simple struct") {
    compile(VParser.struct, "struct Moo { x &Int; }") shouldEqual
        StructP("Moo",MutableP,None,List(),List(StructMemberP("x",FinalP,BorrowPT(NameOrRunePT("Int")))))
  }

  test("Test block's trailing void presence") {
    compile(VParser.filledBody, "{ moo() }") shouldEqual
        Some(BlockPE(List(FunctionCallPE(LookupPE("moo", List()),List(),true))))

    compile(VParser.filledBody, "{ moo(); }") shouldEqual
        Some(BlockPE(List(FunctionCallPE(LookupPE("moo", List()),List(),true), VoidPE())))
  }

  test("ifs") {
    compile(VParser.ifLadder, "if (true) { doBlarks(&x) } else { }") shouldEqual
        IfPE(
          BlockPE(List(BoolLiteralPE(true))),
          BlockPE(List(FunctionCallPE(LookupPE("doBlarks", List()),List(LendPE(LookupPE("x", List()))),true))),
          BlockPE(List(VoidPE())))
  }

  test("Block with only a result") {
    compile(
      VParser.block,
      "= doThings(a);") shouldEqual
        BlockPE(List(FunctionCallPE(LookupPE("doThings", List()),List(LookupPE("a", List())),true)))
  }


  test("Block with statement and result") {
    compile(
      VParser.block,
      """
        |b;
        |= a;
      """.stripMargin) shouldEqual
        BlockPE(List(LookupPE("b", List()), LookupPE("a", List())))
  }


  test("Block with result that could be an expr") {
    // = doThings(a); could be misinterpreted as an expression doThings(=, a) if we're
    // not careful.
    compile(
      VParser.block,
      """
        |a = 2;
        |= doThings(a);
      """.stripMargin) shouldEqual
        BlockPE(
          List(
            LetPE(List(),PatternPP(Some(CaptureP("a",FinalP)),None,None,None),IntLiteralPE(2)),
            FunctionCallPE(LookupPE("doThings", List()),List(LookupPE("a", List())),true)))
  }

  test("Templated impl") {
    compile(
      VParser.impl,
      """
        |impl<T> SomeStruct<T> for MyInterface<T>;
      """.stripMargin) shouldEqual
      ImplP(
        List("T"),
        List(),
        CallPPT(NameOrRunePPT("SomeStruct"),List(NameOrRunePPT("T"))),
        CallPPT(NameOrRunePPT("MyInterface"),List(NameOrRunePPT("T"))))
  }

  test("Impling a template call") {
    compile(
      VParser.impl,
      """
        |impl MyIntIdentity for IFunction1<mut, Int, Int>;
        |""".stripMargin) shouldEqual
      ImplP(
        List(),
        List(),
        NameOrRunePPT("MyIntIdentity"),
        CallPPT(NameOrRunePPT("IFunction1"),List(MutabilityPPT(MutableP), NameOrRunePPT("Int"), NameOrRunePPT("Int"))))
  }


  test("Virtual function") {
    compile(
      VParser.topLevelFunction,
      """
        |fn doCivicDance(virtual this Car) Int;
      """.stripMargin) shouldEqual
      FunctionP(
        Some("doCivicDance"),false,false,true,List(),List(),
        List(PatternPP(Some(CaptureP("this",FinalP)),Some(NameOrRunePPT("Car")),None,Some(AbstractP))),
        Some(NameOrRunePPT("Int")),None)
  }


  test("17") {
    compile(
      VParser.structMember,
      "a *ListNode<T>;") shouldEqual
      StructMemberP("a",FinalP,SharePT(CallPT(NameOrRunePT("ListNode"),List(NameOrRunePT("T")))))
  }

  test("18") {
    compile(
      VParser.structMember,
      "a Array<imm, T>;") shouldEqual
      StructMemberP("a",FinalP,CallPT(NameOrRunePT("Array"),List(MutabilityPT(ImmutableP), NameOrRunePT("T"))))
  }

  test("19") {
    compile(VParser.statement,
      "newLen = if (num == 0) { 1 } else { 2 };") shouldEqual
      LetPE(
        List(),
        PatternPP(Some(CaptureP("newLen",FinalP)),None,None,None),
        IfPE(
          BlockPE(List(FunctionCallPE(LookupPE("==", List()),List(LookupPE("num", List()), IntLiteralPE(0)),true))),
          BlockPE(List(IntLiteralPE(1))),
          BlockPE(List(IntLiteralPE(2)))))
  }

  test("20") {
    compile(VParser.expression,
      "weapon.owner.map()") shouldEqual
      FunctionCallPE(
        DotPE(
          DotPE(LookupPE("weapon",List()),LookupPE("owner",List()),true),
          LookupPE("map",List()),
          true),
        List(),
        true)
  }

  test("!=") {
    compile(VParser.expression,"3 != 4") shouldEqual
      FunctionCallPE(LookupPE("!=",List()),List(IntLiteralPE(3), IntLiteralPE(4)),true)
  }
}
