package net.verdagon.vale.parser

import net.verdagon.vale.parser.patterns.{PatternParser, PatternTemplexParser}
import net.verdagon.vale.parser.rules._
import net.verdagon.vale.vassert

import scala.util.parsing.combinator.RegexParsers

// Rules of thumb:
// - A rule shouldn't match the empty string.
// - A rule shouldn't match whitespace on either side (it makes it hard to require
//   whitespace, like inside the let statement).

object VParser
    extends RuleParser
        with RuleTemplexParser
        with RegexParsers
        with ParserUtils
        with TemplexParser
        with PatternParser
        with PatternTemplexParser
        with ExpressionParser {
  override def skipWhitespace = false
  override val whiteSpace = "[ \t\r\f]+".r

  def filledBody: Parser[Option[BlockPE]] = {
    "{" ~> optWhite ~> block <~ optWhite <~ "}" ^^ {
      case body => Some(body)
    }
  }

  def emptyBody: Parser[Option[BlockPE]] = {
    pos ~ ("{" ~> optWhite ~> "}") ~ pos ^^ {
      case begin ~ body ~ end => Some(BlockPE(Range(begin, end), List(VoidPE())))
    }
  }

  def noBody: Parser[Option[BlockPE]] = {
    ";" ^^^ None
  }

  def maybeBody = filledBody | emptyBody | noBody

  def topLevelFunction: Parser[FunctionP] = {
        pos ~
        existsW("abstract") ~
        existsW("extern") ~
        ("fn" ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (patternPrototypeParams <~ optWhite) ~
        // We have template rules before and after the return type because the return type likes
        // to parse the `rules` in `rules(stuff here)` as a type and then fail when it hits the
        // parentheses.
        opt(templateRulesPR <~ optWhite) ~
        opt(patternTemplex <~ optWhite) ~
        opt(templateRulesPR <~ optWhite) ~
        (maybeBody) ~
        pos ^^ {
      case begin ~ maybeAbstract ~ maybeExtern ~ name ~ identifyingRunes ~ patternParams ~ maybeTemplateRulesBeforeReturn ~ maybeReturnType ~ maybeTemplateRulesAfterReturn ~ maybeBody ~ end => {
        vassert(!(maybeTemplateRulesBeforeReturn.nonEmpty && maybeTemplateRulesAfterReturn.nonEmpty))
        FunctionP(
          Range(begin, end),
          Some(name),
          maybeExtern,
          maybeAbstract,
          identifyingRunes,
          (maybeTemplateRulesBeforeReturn.toList ++ maybeTemplateRulesAfterReturn.toList).headOption,
          Some(patternParams),
          maybeReturnType,
          maybeBody)
      }
    }
  }

  def structMember: Parser[StructMemberP] = {
    (exprIdentifier ~ opt("!") <~ optWhite) ~ (templex <~ optWhite <~ ";") ^^ {
      case name ~ None ~ tyype => StructMemberP(name, FinalP, tyype)
      case name ~ Some(_) ~ tyype => StructMemberP(name, VaryingP, tyype)
    }
  }

  private[parser] def interface: Parser[InterfaceP] = {
    pos ~ (("interface " ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (opt("imm") <~ optWhite) ~
        (opt(templateRulesPR) <~ optWhite <~ "{" <~ optWhite) ~
        repsep(topLevelFunction, optWhite) <~ (optWhite <~ "}")) ~ pos ^^ {
      case begin ~ (name ~ maybeIdentifyingRunes ~ imm ~ maybeTemplateRules ~ members) ~ end => {
        val mutability = if (imm == Some("imm")) ImmutableP else MutableP
        InterfaceP(Range(begin, end), name, mutability, maybeIdentifyingRunes, maybeTemplateRules, members)
      }
    }
  }

  def struct: Parser[StructP] = {
    pos ~ (("struct" ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (opt("export") <~ optWhite) ~
        (opt("imm") <~ optWhite) ~
        (opt(templateRulesPR) <~ optWhite <~ "{" <~ optWhite) ~
        repsep(structMember, optWhite) <~ (optWhite <~ "}")) ~ pos ^^ {
      case begin ~ (name ~ identifyingRunes ~ export ~ imm ~ maybeTemplateRules ~ members) ~ end => {
        val mutability = if (imm == Some("imm")) ImmutableP else MutableP
        StructP(Range(begin, end), name, export.nonEmpty, mutability, identifyingRunes, maybeTemplateRules, members)
      }
    }
  }

  private[parser] def impl: Parser[ImplP] = {
    pos ~ (("impl" ~> optWhite ~>
      opt(identifyingRunesPR <~ optWhite) ~
      opt(templateRulesPR) <~ optWhite) ~
      (patternTemplex <~ optWhite <~ "for" <~ optWhite) ~
      (patternTemplex <~ optWhite <~ ";")) ~ pos ^^ {
      case begin ~ (maybeIdentifyingRunes ~ maybeTemplateRules ~ structType ~ interfaceType) ~ end => {
        ImplP(Range(begin, end), maybeIdentifyingRunes, maybeTemplateRules, structType, interfaceType)
      }
    }
  }

  private[parser] def topLevelThing: Parser[ITopLevelThing] = {
    struct ^^ TopLevelStruct |
    topLevelFunction ^^ TopLevelFunction |
    interface ^^ TopLevelInterface |
    impl ^^ TopLevelImpl
  }

  def program: Parser[Program0] = {
    optWhite ~> repsep(topLevelThing, optWhite) <~ optWhite ^^ Program0
  }

  def runParser(codeWithComments: String): Option[Program0] = {
    val regex = "//[^\\r\\n]*".r
    val code = regex.replaceAllIn(codeWithComments, "")

    VParser.parse(VParser.program, code.toCharArray) match {
      case VParser.NoSuccess(msg, _) => {
        println("No! " + msg)
        None
      }
      case VParser.Success(program0, rest) => {
        vassert(rest.atEnd)
        vassert(rest.offset == code.length)
        Some(program0)
      }
    }
  }
}
