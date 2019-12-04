package net.verdagon.radonc.parser

import scala.util.parsing.combinator.RegexParsers

trait TemplexParser extends RegexParsers with ParserUtils {

  def repeaterSeqTemplex: Parser[ITemplexPT] = {
    (("[" ~> optWhite ~> templex) ~ (optWhite ~> "*" ~> optWhite ~> templex <~ optWhite <~ "]") ^^ {
      case numElements ~ elementType => {
        ArraySequencePT(MutabilityPT(MutableP), numElements, elementType)
      }
    }) |
    (("[:" ~> optWhite ~> unaryTemplex) ~ (optWhite ~> templex) ~ (optWhite ~> "*" ~> optWhite ~> templex <~ optWhite <~ "]") ^^ {
      case mutability ~ numElements ~ elementType => {
        ArraySequencePT(mutability, numElements, elementType)
      }
    })
  }

  private[parser] def unaryTemplex: Parser[ITemplexPT] = {
    ("(" ~> optWhite ~> templex <~ optWhite <~ ")") |
    repeaterSeqTemplex |
    "true" ^^^ BoolPT(true) |
    "false" ^^^ BoolPT(false) |
    "own" ^^^ OwnershipPT(OwnP) |
    "borrow" ^^^ OwnershipPT(BorrowP) |
    "share" ^^^ OwnershipPT(ShareP) |
    "raw" ^^^ OwnershipPT(RawP) |
    "mut" ^^^ MutabilityPT(MutableP) |
    "imm" ^^^ MutabilityPT(ImmutableP) |
    "inl" ^^^ LocationPT(InlineP) |
    "yon" ^^^ LocationPT(YonderP) |
    "xrw" ^^^ PermissionPT(ExclusiveReadwriteP) |
    "rw" ^^^ PermissionPT(ReadwriteP) |
    "ro" ^^^ PermissionPT(ReadonlyP) |
    (typeIdentifier ^^ NamePT)
  }

  private[parser] def templex: Parser[ITemplexPT] = {
    ("#" ~> optWhite ~> typeIdentifier ^^ RunePT) |
    ("?" ~> optWhite ~> templex ^^ NullablePT) |
    ("&" ~> optWhite ~> templex ^^ BorrowPT) |
    ("*" ~> optWhite ~> templex ^^ SharePT) |
    ("^" ~> optWhite ~> templex ^^ OwnPT) |
    (((unaryTemplex <~ optWhite <~ ":" <~ optWhite) ~ templex) ^^ {
      case template ~ arg => CallPT(template, List(arg))
    }) |
    (((unaryTemplex <~ optWhite <~ ":" <~ optWhite) ~ ("(" ~> optWhite ~> repsep(templex, optWhite ~ "," ~ optWhite) <~ optWhite <~ ")")) ^^ {
      case template ~ args => CallPT(template, args)
    }) |
    unaryTemplex
  }

}
