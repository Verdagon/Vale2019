package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser._

import scala.util.parsing.combinator.RegexParsers

trait PatternParser extends PatternTemplexParser with RegexParsers with ParserUtils {

  // Things needed from other parsers
  private[parser] def exprIdentifier: Parser[String]
  private[parser] def rune: Parser[String]


  // Add any new rules to the "Check no parser rules match empty" test!

  // Remember, for pattern parsers, something *must* be present, don't match empty.
  // And it relies on that fact for the subpatterns too.
  private[parser] def atomPattern: Parser[PatternPP] = {
    // This handles when there's a "virtual" in the mix. There can't be a destructure, and there must be a type.
    ((underscoreOr(patternCapture) <~ optWhite <~ ":" <~ optWhite <~ "virtual" <~ optWhite) ~ patternTemplex ^^ {
      case maybeCapture ~ templex => {
        PatternPP(maybeCapture, Some(templex), None, Some(AbstractP))
      }
    }) |
    // Handles when there's a "for" in there somewhere. If there's a "for" then there must be a type.
    (opt(underscoreOr(patternCapture) <~ optWhite) ~ (":" ~> optWhite ~> patternTemplex) ~ opt(optWhite ~> destructure) ~ (optWhite ~> "for" ~> optWhite ~> patternTemplex) ^^ {
      case maybeCapture ~ patternTemplex ~ maybeDestructure ~ overrride => {
        PatternPP(maybeCapture.flatten, Some(patternTemplex), maybeDestructure, Some(OverrideP(overrride)))
      }
    }) |
    // Handles when there's no override or virtual, but there is a type in there.
    ((opt(underscoreOr(patternCapture)) <~ optWhite) ~ (":" ~> optWhite ~> patternTemplex) ~ opt(optWhite ~> destructure) ^^ {
      case maybeCapture ~ patternTemplex ~ maybeDestructure => {
        PatternPP(maybeCapture.flatten, Some(patternTemplex), maybeDestructure, None)
      }
    }) |
    // Handles when there's no override or virtual or type, but there is a capture or a destructure.
    (atLeastOneOfWW(underscoreOr(patternCapture), destructure) ^^ {
      case maybeCapture ~ maybeDestructure => {
        PatternPP(maybeCapture.flatten, None, maybeDestructure, None)
      }
    })
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  // Remember, for pattern parsers, something *must* be present, don't match empty.
  // Luckily, for this rule, we always have the expr identifier.
  private[parser] def patternCapture: Parser[CaptureP] = {
    exprIdentifier ~ opt(optWhite ~> "!") ^^ {
      case name ~ maybeMutable => CaptureP(name, if (maybeMutable.nonEmpty) VaryingP else FinalP)
    }
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  // Remember, for pattern parsers, something *must* be present, don't match empty.
  case class PatternTypePPI(ownership: Option[OwnershipP], rune: Option[String], kind: Option[ITemplexPPT])
  private[parser] def patternType: Parser[PatternTypePPI] = {
    opt(patternOwnership <~ optWhite) ~ runeOrKindPattern ^^ {
      case maybeOwnershipP ~ maybeRuneOrKind => {
        maybeRuneOrKind match {
          case RuneRuneOrKindPPI(rune) => PatternTypePPI(maybeOwnershipP, Some(rune), None)
          case KindRuneOrKindPPI(kind) => PatternTypePPI(maybeOwnershipP, None, Some(kind))
        }
      }
    }
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def destructure: Parser[List[Option[PatternPP]]] = {
    "[" ~> optWhite ~> repsep(underscoreOr(atomPattern), optWhite ~> "," <~ optWhite) <~ optWhite <~ "]"
  }

  sealed trait IRuneOrKindPPI
  case class RuneRuneOrKindPPI(rune: String) extends IRuneOrKindPPI
  case class KindRuneOrKindPPI(kind: ITemplexPPT) extends IRuneOrKindPPI

  private[parser] def runeOrKindPattern: Parser[IRuneOrKindPPI] = {
    (rune ^^ RuneRuneOrKindPPI) |
    (kindPattern ^^ KindRuneOrKindPPI)
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def patternOwnership: Parser[OwnershipP] = {
    // See "Capturing Kinds and Ownerships" for why we don't capture a rune here.
    (("^" ^^^ OwnP) | ("&" ^^^ BorrowP) | ("*" ^^^ ShareP) | ("~" ^^^ RawP))
  }

  // Add any new rules to the "Check no parser rules match empty" test!

  private[parser] def kindPattern: Parser[ITemplexPPT] = {
    patternTemplex
//    callableKindPattern |
//        repeaterSequenceKindPattern |
//        manualSequenceKindPattern |
//        kindPatternAtomic
  }

//  // Atomic means no neighboring, see parser doc.
//  def kindPatternAtomic: Parser[IKindPP] = {
//    templateCallKindPattern | namedKindPattern
//  }
//
//  def templateCallKindPattern: Parser[TemplateCallKindPP] = {
//    templateCallKindFilterRuleAtomic ^^ TemplateCallKindPP
//  }
//
//  def namedKindPattern: Parser[NameTemplexPP] = {
//    typeIdentifier ^^ NameTemplexPP
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!
//
//  private[parser] def callableKindPattern: Parser[CallableKindPP] = {
//    ("(" ~> optWhite ~> repsep(underscoreOr(patternCoordRule), optWhite ~> "," <~ optWhite) <~ optWhite <~ ")" <~ optWhite <~ ":" <~ optWhite) ~ underscoreOr(patternCoordRule) ^^ {
//      case params ~ ret => CallableKindPP(CallableKindFilterPR(params, ret))
//    }
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!
//
//  private[parser] def repeaterSequenceKindPattern: Parser[RepeaterSequenceTemplexPP] = {
//    "[" ~> optWhite ~> (underscoreOr(patternIntRule) <~ optWhite <~ "*" <~ optWhite) ~ underscoreOr(patternCoordRule) <~ optWhite <~ "]" ^^ {
//      case size ~ coordRule => RepeaterSequenceKindPP(RepeaterSequenceTemplexPR(size, coordRule))
//    }
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!
//
//  private[parser] def manualSequenceKindPattern: Parser[ManualSequenceTemplexPP] = {
//    "[" ~> optWhite ~> repsep(underscoreOr(patternCoordRule), optWhite <~ "," <~ optWhite) <~ optWhite <~ "]" ^^ {
//      case elements => ManualSequenceTemplexPP(ManualSequenceTemplexPR(Some(elements)))
//    }
//  }

  // Add any new rules to the "Check no parser rules match empty" test!

//  private[parser] def patternCoordRule: Parser[CoordPR] = {
//    opt(patternOwnership <~ optWhite) ~ underscoreOr(atLeastOneOfWW(rune, patternKindRule)) ^^ {
//      case maybeOwnership ~ None => {
//        CoordPR(
//          None,
//          CoordPR(
//            maybeOwnership.map(a => OwnershipPR(None, OwnershipPR(maybeOwnership.map(a => Set(a))))),
//            None))
//      }
//      case maybeOwnership ~ Some(maybeRune ~ maybeKind) => {
//        CoordPR(
//          maybeRune,
//          CoordPR(
//            maybeOwnership.map(a => OwnershipPR(None, OwnershipPR(maybeOwnership.map(a => Set(a))))),
//            maybeKind.map(k => KindPR(None, k))))
//      }
//    }
//  }

  // Add any new rules to the "Check no parser rules match empty" test!

//  private[parser] def patternIntRule: Parser[IntPR] = {
//    atLeastOneOfWW(rune, int) ^^ {
//      case maybeRune ~ maybeInt => {
//        IntPR(maybeRune, IntPR(maybeInt.map(int => Set(int))))
//      }
//    }
//  }

  // Add any new rules to the "Check no parser rules match empty" test!

//  // Like the "Moo" in "a: &Moo", we're parsing the kind in a pattern.
//  private[parser] def patternKindRule: Parser[IKindTypePR] = {
//    // When you add anything here, update kindPattern too.
//    // When you add anything here, update kindFilterRule too, though not everything in there will be in here.
//    (patternCallableKindRule |
//        patternRepeaterSequenceKindRule |
//        patternManualSequenceKindRule |
//        patternTemplateCallKindFilterRule |
//        (typeIdentifier ^^ NamedKindPR))
//    // When you add anything here, update kindPattern too.
//    // When you add anything here, update kindFilterRule too, though not everything in there will be in here.
//  }

//  // Atomic means no neighboring, see parser doc.
//  private[parser] def patternKindRuleInner: Parser[KindPR] = {
////    (patternTemplateCallKindFilterRule ^^ (t => KindPR(None, KindPR(Some(t))))) |
//        ((typeIdentifier ^^ NamedKindPR) ^^ (a => KindPR(None, a)))
//  }
//
//  private[parser] def patternCallableKindRule: Parser[CallableKindFilterPR] = {
//    ("(" ~> optWhite ~> repsep(patternCoordRule, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")" <~ optWhite <~ ":" <~ optWhite) ~ patternCoordRule ^^ {
//      case params ~ ret => CallableKindFilterPR(params.map(p => Some(p)), Some(ret))
//    }
//  }
//
//  private[parser] def patternTemplateCallKindFilterRule: Parser[TemplateCallKindFilterPR] = {
//    (((underscoreOr(patternKindRuleInner) <~ optWhite <~ ":" <~ optWhite) ~ underscoreOr(runeableTemplataRuleAtomic)) ^^ {
//      case template ~ arg => TemplateCallKindFilterPR(template, List(arg))
//    }) |
//    (((underscoreOr(patternKindRuleInner) <~ optWhite <~ ":" <~ optWhite <~ "(" <~ optWhite) ~ repsep(underscoreOr(runeableTemplataRule), optWhite ~> "," <~ optWhite) <~ optWhite <~ ")") ^^ {
//      case template ~ args => TemplateCallKindFilterPR(template, args)
//    })
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!
//
//  private[parser] def patternManualSequenceKindRule: Parser[ManualSequenceTemplexPR] = {
//    "[" ~> optWhite ~> repsep(patternCoordRule, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]" ^^ {
//      case elements => ManualSequenceTemplexPR(Some(elements.map(e => Some(e))))
//    }
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!
//
//  private[parser] def patternRepeaterSequenceKindRule: Parser[RepeaterSequenceTemplexPR] = {
//    "[" ~> optWhite ~> (underscoreOr(patternIntRule) <~ optWhite <~ "*" <~ optWhite) ~ underscoreOr(patternCoordRule) <~ optWhite <~ "]" ^^ {
//      case size ~ coordRule => RepeaterSequenceTemplexPR(size, coordRule)
//    }
//  }
//
//  // Add any new rules to the "Check no parser rules match empty" test!

}
