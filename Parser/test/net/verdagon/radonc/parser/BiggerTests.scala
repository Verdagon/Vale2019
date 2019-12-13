package net.verdagon.radonc.parser

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

  test("Empty lambda") {
    compile(VParser.expression, "{}") shouldEqual VParser.emptyParamlessLambda(true)
  }

  test("Simple while loop") {
    compile(VParser.statement,"while {} {}") shouldEqual
        WhilePE(BlockPE(List(VoidPE())), BlockPE(List(VoidPE())))
  }

  test("Result after while loop") {
    compile(VParser.block,"while {} {} = false;") shouldEqual
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

  test("Function call") {
    val program = compile(VParser.program, "fn main(){call(sum)}")
    program shouldHave FunctionCallPE(LookupPE("call", List()),PackPE(List(LookupPE("sum", List()))),true)
  }

  test("Test templated lambda param") {
    val program = compile(VParser.program, "fn main(){{(a) a + a}(3)}")
    program shouldHaveMatch { case FunctionCallPE(LambdaPE(_), PackPE(List(IntLiteralPE(3))),true) => }
    program shouldHave PatternPP(Some(CaptureP("a",FinalP)),None,None,None)
    program shouldHave FunctionCallPE(LookupPE("+", List()),PackPE(List(LookupPE("a", List()), LookupPE("a", List()))),true)
  }

  test("Simple struct") {
    compile(VParser.struct, "struct Moo { x: &Int; }") shouldEqual
        StructP("Moo",MutableP,None,List(),List(StructMemberP("x",FinalP,BorrowPT(NamePT("Int")))))
  }

  test("Test block's trailing void presence") {
    compile(VParser.filledBody, "{ moo() }") shouldEqual
        Some(BlockPE(List(FunctionCallPE(LookupPE("moo", List()),PackPE(List()),true))))

    compile(VParser.filledBody, "{ moo(); }") shouldEqual
        Some(BlockPE(List(FunctionCallPE(LookupPE("moo", List()),PackPE(List()),true), VoidPE())))
  }

  test("ifs") {
    compile(VParser.ifLadder, "if {true} { doBlarks(&x) } else { }") shouldEqual
        IfPE(
          BlockPE(List(BoolLiteralPE(true))),
          BlockPE(List(FunctionCallPE(LookupPE("doBlarks", List()),PackPE(List(LendPE(LookupPE("x", List())))),true))),
          BlockPE(List(VoidPE())))
  }

  test("Block with only a result") {
    compile(
      VParser.block,
      "= doThings(a);") shouldEqual
        BlockPE(List(FunctionCallPE(LookupPE("doThings", List()),PackPE(List(LookupPE("a", List()))),true)))
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
            FunctionCallPE(LookupPE("doThings", List()),PackPE(List(LookupPE("a", List()))),true)))
  }

  test("Templated impl") {
    compile(
      VParser.impl,
      """
        |impl SomeStruct<T> for MyInterface<T>;
      """.stripMargin) shouldEqual
      ImplP(
        List(),
        CallPPT(NamePPT("SomeStruct"),List(NamePPT("T"))),
        CallPPT(NamePPT("MyInterface"),List(NamePPT("T"))))
  }

  test("Impling a template call") {
    compile(
      VParser.impl,
      """
        |impl MyIntIdentity for IFunction1<mut, Int, Int>;
        |""".stripMargin) shouldEqual
      ImplP(
        List(),
        NamePPT("MyIntIdentity"),
        CallPPT(NamePPT("IFunction1"),List(MutabilityPPT(MutableP), NamePPT("Int"), NamePPT("Int"))))
  }


  test("Virtual function") {
    compile(
      VParser.topLevelFunction,
      """
        |fn doCivicDance(this: virtual Car) Int;
      """.stripMargin) shouldEqual
      FunctionP(
        Some("doCivicDance"),false,false,true,List(),List(),
        List(PatternPP(Some(CaptureP("this",FinalP)),Some(NamePPT("Car")),None,Some(AbstractP))),
        Some(NamePPT("Int")),None)
  }


  test("17") {
    compile(
      VParser.structMember,
      "a: *ListNode<T>;") shouldEqual
      StructMemberP("a",FinalP,SharePT(CallPT(NamePT("ListNode"),List(NamePT("T")))))
  }

  test("18") {
    compile(
      VParser.structMember,
      "a: __Array<imm, #T>;") shouldEqual
      StructMemberP("a",FinalP,CallPT(NamePT("__Array"),List(MutabilityPT(ImmutableP), RunePT("T"))))
  }

  test("19") {
    compile(VParser.statement,
      "newLen = if {num == 0} { 1 } else { 2 };") shouldEqual
      LetPE(
        List(),
        PatternPP(Some(CaptureP("newLen",FinalP)),None,None,None),
        IfPE(
          BlockPE(List(FunctionCallPE(LookupPE("==", List()),PackPE(List(LookupPE("num", List()), IntLiteralPE(0))),true))),
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
        PackPE(List()),
        true)
  }

  test("!=") {
    // This is why we can't disallow ! and = in infixFunctionIdentifier.
    compile(VParser.infixFunctionIdentifier,"!=") shouldEqual "!="
  }
}
