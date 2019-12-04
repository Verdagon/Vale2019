package net.verdagon.radonc.parser

import org.scalatest.{FunSuite, Matchers}


class StructTests extends FunSuite with Matchers with Collector {
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

  test("Struct with rune") {
    compile(VParser.topLevelThing,
      """
        |struct ListNode:#E {
        |  value: E;
        |  next: ListNode:E;
        |}
      """.stripMargin) shouldEqual
          TopLevelStruct(
            StructP(
              "ListNode",
              MutableP,
              Some(List("E")),
              List(),
              List(
                StructMemberP("value",FinalP,NamePT("E")),
                StructMemberP("next",FinalP,CallPT(NamePT("ListNode"),List(NamePT("E")))))))
  }

  test("Struct with int rune") {
    compile(VParser.topLevelThing,
      """
        |struct Vecf:#N
        |rules(#N: Int)
        |{
        |  values: [N * Float];
        |}
        |
      """.stripMargin) shouldEqual
        TopLevelStruct(
          StructP(
            "Vecf",
            MutableP,
            Some(List("N")),
            List(TypedPR(Some("N"),IntTypePR)),
            List(StructMemberP("values",FinalP,ArraySequencePT(MutabilityPT(MutableP), NamePT("N"), NamePT("Float"))))))
  }

  test("Struct with int rune, array sequence specifies mutability") {
    compile(VParser.topLevelThing,
      """
        |struct Vecf:#N
        |rules(#N: Int)
        |{
        |  values: [:imm N * Float];
        |}
        |
      """.stripMargin) shouldEqual
      TopLevelStruct(
        StructP(
          "Vecf",
          MutableP,
          Some(List("N")),
          List(TypedPR(Some("N"),IntTypePR)),
          List(StructMemberP("values",FinalP,ArraySequencePT(MutabilityPT(ImmutableP), NamePT("N"), NamePT("Float"))))))
  }
}
