package net.verdagon.radonc.parser

import net.verdagon.radonc.parser.patterns.{PatternParser, PatternTemplexParser}
import net.verdagon.radonc.parser.rules._
import net.verdagon.radonc.vassert

import scala.util.parsing.combinator.RegexParsers

// Rules of thumb:
// - A rule shouldn't match the empty string.
// - A rule shouldn't match whitespace on either side (it makes it hard to require
//   whitespace, like inside the let statement).

sealed trait TopLevelThingP
case class TopLevelFunction(function: FunctionP) extends TopLevelThingP
case class TopLevelStruct(struct: StructP) extends TopLevelThingP
case class TopLevelInterface(function: InterfaceP) extends TopLevelThingP
case class TopLevelImplements(impl: ImplP) extends TopLevelThingP

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
    "{" ~> optWhite ~> "}" ^^ {
      case body => Some(BlockPE(List(VoidPE())))
    }
  }

  def noBody: Parser[Option[BlockPE]] = {
    ";" ^^^ None
  }

  def maybeBody = filledBody | emptyBody | noBody

  def topLevelFunction: Parser[FunctionP] = positioned {
        opt("abstract" <~ optWhite) ~
        opt("extern" <~ optWhite) ~
        ("fn" ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (patternPrototypeParams <~ optWhite) ~
        // We have template rules before and after the return type because the return type likes
        // to parse the `rules` in `rules(stuff here)` as a type and then fail when it hits the
        // parentheses.
        opt(templateRulesPR <~ optWhite) ~
        opt(patternTemplex <~ optWhite) ~
        opt(templateRulesPR <~ optWhite) ~
        (maybeBody) ^^ {
      case maybeAbstract ~ maybeExtern ~ name ~ identifyingRunes ~ patternParams ~ maybeTemplateRulesBeforeReturn ~ maybeReturnType ~ maybeTemplateRulesAfterReturn ~ maybeBody => {
        vassert(!(maybeTemplateRulesBeforeReturn.nonEmpty && maybeTemplateRulesAfterReturn.nonEmpty))
        FunctionP(
          Some(name),
          maybeExtern.nonEmpty,
          maybeAbstract.nonEmpty,
          true,
          identifyingRunes.getOrElse(List()),
          maybeTemplateRulesBeforeReturn.getOrElse(maybeTemplateRulesAfterReturn.getOrElse(List())),
          patternParams,
          maybeReturnType,
          maybeBody)
      }
    }
  }

  def structMember: Parser[StructMemberP] = {
    (((exprIdentifier <~ optWhite) ~ opt("!" <~ optWhite)) <~ ":" <~ optWhite) ~ (templex <~ optWhite <~ ";") ^^ {
      case name ~ None ~ tyype => StructMemberP(name, FinalP, tyype)
      case name ~ Some(_) ~ tyype => StructMemberP(name, VaryingP, tyype)
    }
  }

  private[parser] def interface: Parser[InterfaceP] = positioned {
    ("interface " ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (opt("imm") <~ optWhite) ~
        (opt(templateRulesPR) <~ optWhite <~ "{" <~ optWhite) ~
        repsep(topLevelFunction, optWhite) <~ (optWhite <~ "}") ^^ {
      case name ~ maybeIdentifyingRunes ~ imm ~ maybeTemplateRules ~ members => {
        val mutability = if (imm == Some("imm")) ImmutableP else MutableP
        InterfaceP(name, mutability, maybeIdentifyingRunes, maybeTemplateRules.getOrElse(List()), members)
      }
    }
  }

  def struct: Parser[StructP] = positioned {
    ("struct" ~> optWhite ~> exprIdentifier <~ optWhite) ~
        opt(identifyingRunesPR <~ optWhite) ~
        (opt("imm") <~ optWhite) ~
        (opt(templateRulesPR) <~ optWhite <~ "{" <~ optWhite) ~
        repsep(structMember, optWhite) <~ (optWhite <~ "}") ^^ {
      case name ~ identifyingRunes ~ imm ~ maybeTemplateRules ~ members => {
        val mutability = if (imm == Some("imm")) ImmutableP else MutableP
        StructP(name, mutability, identifyingRunes, maybeTemplateRules.getOrElse(List()), members)
      }
    }
  }

  private[parser] def impl: Parser[ImplP] = positioned {
     ("impl" ~> optWhite ~>
      opt(templateRulesPR) <~ optWhite) ~ (patternTemplex <~ optWhite <~ "for" <~ optWhite) ~ (patternTemplex <~ optWhite <~ ";") ^^ {
      case maybeTemplateRules ~ structType ~ interfaceType => {
        ImplP(maybeTemplateRules.getOrElse(List()), structType, interfaceType)
      }
    }
  }

  private[parser] def topLevelThing: Parser[TopLevelThingP] = {
    struct ^^ TopLevelStruct |
    topLevelFunction ^^ TopLevelFunction |
    interface ^^ TopLevelInterface |
    impl ^^ TopLevelImplements
  }

  def program: Parser[Program0] = {
    optWhite ~> repsep(topLevelThing, optWhite) <~ optWhite ^^ {
      case tlts => {
        val structs: List[StructP] =
          tlts.collect({ case TopLevelStruct(s) => s });
        val interfaces: List[InterfaceP] =
          tlts.collect({ case TopLevelInterface(i) => i});
        val impls: List[ImplP] =
          tlts.collect({ case TopLevelImplements(i) => i });
        val functions: List[FunctionP] =
          tlts.collect({ case TopLevelFunction(f) => f });
        Program0(structs, interfaces, impls, functions)
      }
    }
  }

  def emptyParamlessLambda(userDefined: Boolean) =
    LambdaPE(FunctionP(None,false,false,userDefined,List(),List(), List(),None,Some(BlockPE(List(VoidPE())))))


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
