package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser._

import scala.util.parsing.combinator.RegexParsers

trait PatternTemplexParser extends RegexParsers with ParserUtils {
  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def nameOrRunePatternTemplex: Parser[NameOrRunePPT] = {
    typeIdentifier ^^ NameOrRunePPT
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def patternTemplex: Parser[ITemplexPPT] = {
    ownershippedTemplex |
    callablePatternTemplex |
    manualSeqPatternTemplex |
    repeaterSeqPatternTemplex |
    ((nameOrRunePatternTemplex <~ optWhite <~ "<" <~ optWhite) ~ repsep(patternTemplex, optWhite ~> "," <~ optWhite) <~ optWhite <~ ">" ^^ {
      case template ~ args => CallPPT(template, args)
    }) |
    (int ^^ (value => IntPPT(value))) |
    ("mut" ^^^ MutabilityPPT(MutableP)) |
    ("imm" ^^^ MutabilityPPT(ImmutableP)) |
    ("true" ^^^ BoolPPT(true)) |
    ("false" ^^^ BoolPPT(false)) |
    "_" ^^^ AnonymousRunePPT() |
    nameOrRunePatternTemplex
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def ownershippedTemplex: Parser[ITemplexPPT] = {
    ("&" ~> optWhite ~> patternTemplex) ^^ (templex => OwnershippedPPT(BorrowP, templex)) |
    ("*" ~> optWhite ~> patternTemplex) ^^ (templex => OwnershippedPPT(ShareP, templex)) |
    ("^" ~> optWhite ~> patternTemplex) ^^ (templex => OwnershippedPPT(OwnP, templex))
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def manualSeqPatternTemplex: Parser[ITemplexPPT] = {
    ("[" ~> optWhite ~> repsep(patternTemplex, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]") ^^ {
      case members => ManualSequencePPT(members)
    }
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def repeaterSeqPatternTemplex: Parser[ITemplexPPT] = {
    (("[" ~> optWhite ~> patternTemplex <~ optWhite <~ "*" <~ optWhite) ~ (patternTemplex <~ optWhite <~ "]") ^^ {
      case size ~ element => RepeaterSequencePPT(MutabilityPPT(MutableP), size, element)
    }) |
    ((("[<" ~> optWhite ~> patternTemplex <~ optWhite <~ ">") ~ (optWhite ~> patternTemplex) <~ optWhite <~ "*" <~ optWhite) ~ (patternTemplex <~ optWhite <~ "]") ^^ {
      case mutability ~ size ~ element => RepeaterSequencePPT(mutability, size, element)
    })
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!

  private[parser] def callablePatternTemplex: Parser[ITemplexPPT] = {
    ("fn" ~> optWhite ~> opt(":" ~> optWhite ~> patternTemplex <~ optWhite)) ~ ("(" ~> optWhite ~> repsep(patternTemplex, optWhite ~ "," ~ optWhite) <~ optWhite <~ ")") ~ (optWhite ~> patternTemplex) ^^ {
      case mutability ~ params ~ ret => FunctionPPT(mutability, params, ret)
    }
  }

  // Add any new patterns to the "Check no parser patterns match empty" test!
}
