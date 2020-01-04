package net.verdagon.vale.parser

import scala.util.parsing.combinator.RegexParsers

trait ParserUtils extends RegexParsers {

  private[parser] def white: Parser[Unit] = { "\\s+".r ^^^ Unit }
  private[parser] def optWhite: Parser[Unit] = { opt(white) ^^^ Unit }

  // soon, give special treatment to ^
  // we want marine^.item to explode marine and extract its item
  // but, we don't want it to parse it as (marine^).item
  // so, we need to look ahead a bit and see if there's a . after it.
  private[parser] def exprIdentifier: Parser[String] = {
    """[^\s\.\!\$\&\,\:\(\)\;\[\]\{\}\'\^\"\<\>\=\`]+""".r
  }

  private[parser] def infixFunctionIdentifier: Parser[String] = {
    """[^\s\.\$\&\,\:\(\)\;\[\]\{\}\'\"\<\>\=\`]+""".r
  }

  private[parser] def typeIdentifier: Parser[String] = {
    """[^\s\.\!\*\?\#\$\&\,\:\|\;\(\)\[\]\{\}=\<\>\`]+""".r
  }

  private[parser] def stringOr[T](string: String, parser: Parser[T]): Parser[Option[T]] = {
    (string ^^^ { val x: Option[T] = None; x } | parser ^^ (a => Some(a)))
  }

  private[parser] def underscoreOr[T](parser: Parser[T]): Parser[Option[T]] = {
    ("_" ^^^ { val x: Option[T] = None; x } | parser ^^ (a => Some(a)))
  }

  private[parser] def int: Parser[Int] = {
    raw"^-?\d+".r ^^ {
      case thingStr => thingStr.toInt
    }
  }

  private[parser] def integer: Parser[IExpressionPE] = {
    int ^^ IntLiteralPE
  }

  private[parser] def float: Parser[IExpressionPE] = {
    raw"^-?\d+\.\d*".r ^^ {
      case thingStr => FloatLiteralPE(thingStr.toFloat)
    }
  }

  private[parser] def string: Parser[IExpressionPE] = {
    "\"" ~> "[^\"]*".r <~ "\"" ^^ {
      case thingStr => StrLiteralPE(thingStr)
    }
  }

  // ww = with whitespace

  private[parser] def atLeastOneOfWW[A, B](
      parserA: Parser[A],
      parserB: Parser[B]
  ): Parser[(Option[A] ~ Option[B])] = {
    // With A definitely present (or both)
    (parserA ~ opt(optWhite ~> parserB) ^^ { case (a ~ maybeB) =>
      val maybeA: Option[A] = Some(a)
      new ~(maybeA, maybeB)
    }) |
        // With B definitely present
        (parserB ^^ { case b => (new ~(None, Some(b))) })
  }

  private[parser] def atLeastOneOfWW[A, B, C](
      parserA: Parser[A],
      parserB: Parser[B],
      parserC: Parser[C]
  ): Parser[(Option[A] ~ Option[B] ~ Option[C])] = {
    atLeastOneOfWW(atLeastOneOfWW(parserA, parserB), parserC) ^^ {
      case (None ~ c) => (new ~(new ~(None, None), c))
      case (Some((a ~ b)) ~ c) => (new ~(new ~(a, b), c))
    }
  }

  private[parser] def atLeastOneOf[A, B](
      parserA: Parser[A],
      parserB: Parser[B]
  ): Parser[(Option[A] ~ Option[B])] = {
    (parserA ~ opt(parserB) ^^ { case (a ~ maybeB) =>
      val maybeA: Option[A] = Some(a)
      new ~(maybeA, maybeB)
    }) |
      // With B definitely present
      (parserB ^^ { case b => (new ~(None, Some(b))) })
  }

  private[parser] def atLeastOneOf[A, B, C, D](
    parserA: Parser[A],
    parserB: Parser[B],
    parserC: Parser[C],
    parserD: Parser[D]
  ): Parser[(Option[A] ~ Option[B] ~ Option[C] ~ Option[D])] = {
    atLeastOneOfWW(atLeastOneOfWW(parserA, parserB, parserC), parserD) ^^ {
      case (None ~ c) => (new ~(new ~(new ~(None, None), None), c))
      case (Some((a ~ b ~ c)) ~ d) => (new ~(new ~(new ~(a, b), c), d))
    }
  }

  private[parser] def onlyOneOf[A, B](
      parserA: Parser[A],
      parserB: Parser[B]
  ): Parser[(Option[A], Option[B])] = {
    (parserA ^^ { case a =>
      val maybeA: Option[A] = Some(a)
      val maybeB: Option[B] = None
      (maybeA, maybeB)
    }) |
        (parserB ^^ { case a =>
          val maybeA: Option[A] = None
          val maybeB: Option[B] = Some(a)
          (maybeA, maybeB)
        })
  }

}
