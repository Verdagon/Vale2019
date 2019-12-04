package net.verdagon.radonc.parser.rules

import net.verdagon.radonc.parser._
import org.scalatest.Ignore

import scala.util.parsing.combinator.RegexParsers

trait RuleParser extends RegexParsers with ParserUtils {

  private[parser] def ruleTemplexPR: Parser[ITemplexPRT]

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def level0PR: Parser[IRulexPR] = {
    ruleTemplexPR ^^ TemplexPR
  }

  private[parser] def typedPR: Parser[TypedPR] = {
    (opt(underscoreOr(rune)) <~ optWhite <~ ":" <~ optWhite) ~ typePR ^^ {
      case thing ~ tyype => {
        TypedPR(thing.flatten, tyype)
      }
    }
  }

  private[parser] def typePR: Parser[ITypePR] = {
    "Ownership" ^^^ OwnershipTypePR |
    "Mutability" ^^^ MutabilityTypePR |
    "Permission" ^^^ PermissionTypePR |
    "Location" ^^^ LocationTypePR |
    "Ref" ^^^ CoordTypePR |
//    "Struct" ^^^ StructTypePR |
//    "Seq" ^^^ SequenceTypePR |
//    "Callable" ^^^ CallableTypePR |
//    "Interface" ^^^ InterfaceTypePR |
    // Int must be after Interface, otherwise we'll have a hanging "erface"
    // Same with Kint and KindTemplate
    "Int" ^^^ IntTypePR |
    "Kind" ^^^ KindTypePR
  }

  private[parser] def destructurePR: Parser[IRulexPR] = {
    (typedPR <~ optWhite <~ "[" <~ optWhite) ~
        repsep(rulePR, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]" ^^ {
      case container ~ components => ComponentsPR(container, components)
    }
  }

  private[parser] def dotPR(innerRule: Parser[IRulexPR]): Parser[IRulexPR] = {
    (innerRule <~ optWhite <~ "." <~ optWhite) ~ typeIdentifier ^^ {
      case inner ~ name => DotPR(inner, name)
    }
  }

  private[parser] def orPR(inner: Parser[IRulexPR]): Parser[IRulexPR] = {
    (inner <~ optWhite <~ "|" <~ optWhite) ~ rep1sep(inner, optWhite ~> "|" <~ optWhite) ^^ {
      case firstPossibility ~ restPossibilities => OrPR(firstPossibility :: restPossibilities)
    }
  }

  private[parser] def level1PR: Parser[IRulexPR] = {
    typedPR | level0PR
  }

  private[parser] def level2PR: Parser[IRulexPR] = {
    destructurePR | level1PR
  }

  private[parser] def level3PR: Parser[IRulexPR] = {
    dotPR(level2PR) | level2PR
  }

  private[parser] def level4PR: Parser[IRulexPR] = {
    orPR(level3PR) | level3PR
  }

  private[parser] def level5PR: Parser[IRulexPR] = {
    implementsPR |
    existsPR |
    equalsPR(level4PR) |
    level4PR
  }

  private[parser] def rulePR: Parser[IRulexPR] = {
    level5PR
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def identifyingRunesPR: Parser[List[String]] = {
    (":" ~> optWhite ~> rune ^^ (r => List(r))) |
    (":" ~> optWhite ~> "(" ~> optWhite ~> repsep(rune, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")")
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  def templateRulesPR: Parser[List[IRulexPR]] = {
    ("rules" ~> optWhite ~> "(" ~> optWhite ~> repsep(rulePR, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")")
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  // Atomic means no neighboring, see parser doc.
  private[parser] def implementsPR: Parser[IRulexPR] = {
    ("implements" ~> optWhite ~> "(" ~> optWhite ~> rulePR <~ optWhite <~ "," <~ optWhite) ~
        (rulePR <~ optWhite <~ ")") ^^ {
      case struct ~ interface => CallPR("implements", List(struct, interface))
    }
  }

  // Atomic means no neighboring, see parser doc.
  private[parser] def existsPR: Parser[IRulexPR] = {
    "exists" ~> optWhite ~> "(" ~> optWhite ~> rulePR <~ optWhite <~ ")" ^^ {
      case thing => CallPR("exists", List(thing))
    }
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def equalsPR(inner: Parser[IRulexPR]): Parser[EqualsPR] = {
    (inner <~ optWhite <~ "=" <~ optWhite) ~ inner ^^ {
      case left ~ right => EqualsPR(left, right)
    }
  }

  // Add any new rules to the "Check no parser rules match empty" test!
}
