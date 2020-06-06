package net.verdagon.vale.parser

import net.verdagon.vale.parser.patterns.PatternParser
import net.verdagon.vale.{vassert, vcheck, vfail}

import scala.util.parsing.combinator.RegexParsers

trait ExpressionParser extends RegexParsers with ParserUtils {
  private[parser] def templex: Parser[ITemplexPT]
  private[parser] def templateRulesPR: Parser[TemplateRulesP]
  private[parser] def atomPattern: Parser[PatternPP]
  private[parser] def patternTemplex: Parser[ITemplexPPT]

  private[parser] def specialOperators: Parser[StringP] = {
    (pstr("<=>") | pstr("<=") | pstr("<") | pstr(">=") | pstr(">") | pstr("==") | pstr("!="))
  }

  private[parser] def lookup: Parser[LookupPE] = {
//    ("`" ~> "[^`]+".r <~ "`" ^^ (i => LookupPE(i, List()))) |
    (exprIdentifier ^^ (i => LookupPE(i, None)))
  }

  private[parser] def templateArgs: Parser[TemplateArgsP] = {
    pos ~ ("<" ~> optWhite ~> repsep(templex, optWhite ~> "," <~ optWhite) <~ optWhite <~ ">") ~ pos ^^ {
      case begin ~ args ~ end => {
        TemplateArgsP(Range(begin, end), args)
      }
    }
  }

  private[parser] def templateSpecifiedLookup: Parser[LookupPE] = {
    (exprIdentifier <~ optWhite) ~ templateArgs ^^ {
      case name ~ templateArgs => LookupPE(name, Some(templateArgs))
    }
  }

  private[parser] def let: Parser[LetPE] = {
//    (opt(templateRulesPR) <~ optWhite) ~
        pos ~
        (atomPattern <~ white <~ "=" <~ white) ~
        (expression <~ optWhite <~ ";") ~
        pos ^^ {
      case begin ~ /*maybeTemplateRules ~*/ pattern ~ expr ~ end => {
        // We just threw away the topLevelRunes because let statements cant have them.
        LetPE(Range(begin, end), /*maybeTemplateRules.getOrElse(List())*/List(), pattern, expr)
      }
    }
  }

  private[parser] def lend: Parser[IExpressionPE] = {
    "&" ~> optWhite ~> postfixableExpressions ^^ LendPE
  }

  private[parser] def not: Parser[IExpressionPE] = {
    pos ~ (pstr("not") <~ optWhite) ~ postfixableExpressions ~ pos ^^ {
      case begin ~ not ~ expr ~ end => {
        FunctionCallPE(Range(begin, end), LookupPE(not, None), List(expr), true)
      }
    }
  }

  private[parser] def ret: Parser[IExpressionPE] = {
    pos ~ ("ret" ~> optWhite ~> expression) ~ pos ^^ {
      case begin ~ sourceExpr ~ end => ReturnPE(Range(begin, end), sourceExpr)
    }
  }

  private[parser] def mutate: Parser[IExpressionPE] = {
    pos ~ ("mut" ~> white ~> expression <~ white <~ "=" <~ white) ~ expression ~ pos ^^ {
      case begin ~ destinationExpr ~ sourceExpr ~ end => MutatePE(Range(begin, end), destinationExpr, sourceExpr)
    }
  }

  private[parser] def swap: Parser[IExpressionPE] = {
    ("exch" ~> optWhite ~> (expression <~ optWhite <~ "," <~ optWhite) ~ (expression <~ optWhite)) ^^ {
      case leftExpr ~ rightExpr => SwapPE(leftExpr, rightExpr)
    }
  }

  private[parser] def bracedBlock: Parser[BlockPE] = {
    pos ~ ("{" ~> optWhite ~> blockExprs <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ exprs ~ end => BlockPE(Range(begin, end), exprs)
    }
  }

  private[parser] def eachOrEachI: Parser[FunctionCallPE] = {
    pos ~ (pstr("eachI") | pstr("each")) ~ (white ~> postfixableExpressions <~ white) ~ lambda ~ pos ^^ {
      case begin ~ eachI ~ collection ~ lam ~ end => {
        FunctionCallPE(Range(begin, end), LookupPE(eachI, None), List(collection, lam), true)
      }
    }
  }

  private[parser] def whiile: Parser[WhilePE] = {
    ("while" ~> optWhite ~> pos) ~ ("(" ~> optWhite ~> blockExprs <~ optWhite <~ ")") ~ (pos <~ optWhite) ~ bracedBlock ^^ {
      case condBegin ~ condExprs ~ condEnd ~ thenBlock => {
        WhilePE(BlockPE(Range(condBegin, condEnd), condExprs), thenBlock)
      }
    }
  }

  private[parser] def ifPart: Parser[(BlockPE, BlockPE)] = {
    ("if" ~> optWhite ~> pos) ~ ("(" ~> optWhite ~> expression <~ optWhite <~ ")") ~ (pos <~ optWhite) ~ bracedBlock ^^ {
      case condBegin ~ condExpr ~ condEnd ~ thenLambda => {
        (BlockPE(Range(condBegin, condEnd), List(condExpr)), thenLambda)
      }
    }
  }

  def ifLadder: Parser[IExpressionPE] = {
    pos ~
        ifPart ~
        rep(optWhite ~> "else" ~> optWhite ~> ifPart) ~
        opt(optWhite ~> "else" ~> optWhite ~> bracedBlock) ~
        pos ^^ {
      case begin ~ rootIf ~ ifElses ~ maybeElseBlock ~ end => {
        val finalElse: BlockPE =
          maybeElseBlock match {
            case None => BlockPE(Range(end, end), List(VoidPE(Range(end, end))))
            case Some(block) => block
          }
        val rootElseBlock =
          ifElses.foldRight(finalElse)({
            case ((condBlock, thenBlock), elseBlock) => {
              BlockPE(
                Range(condBlock.range.begin, thenBlock.range.end),
                List(
                  IfPE(
                    Range(condBlock.range.begin, thenBlock.range.end),
                    condBlock, thenBlock, elseBlock)))
            }
          })
        val (rootConditionLambda, rootThenLambda) = rootIf
        IfPE(Range(begin, end), rootConditionLambda, rootThenLambda, rootElseBlock)
      }
    }
  }

  def statement: Parser[IExpressionPE] = {
    // bracedBlock is at the end because we want to be able to parse "{print(_)}(4);" as an expression.
    // debt: "block" is here temporarily because we get ambiguities in this case:
    //   fn main() { {_ + _}(4 + 5) }
    // because it mistakenly successfully parses {_ + _} then dies on the next part.
    (mutate <~ ";") | swap | let | whiile | eachOrEachI | ifLadder | (expression <~ ";") | ("block" ~> optWhite ~> bracedBlock)
  }

  private[parser] def expressionElementLevel1: Parser[IExpressionPE] = {
    stringExpr |
      integer |
      bool |
      lambda |
      (packExpr ^^ {
        case List(only) => only
        case _ => vfail()
      }) |
      tupleExpr |
      templateSpecifiedLookup |
      (lookup ^^ {
        case l @ LookupPE(StringP(r, "_"), None) => MagicParamLookupPE(r)
        case other => other
      })
  }

  private[parser] def expressionLevel9: Parser[IExpressionPE] = {

    sealed trait IStep
    case class MethodCallStep(borrowContainer: Boolean, lookup: LookupPE, args: List[IExpressionPE]) extends IStep
    case class MemberAccessStep(name: LookupPE) extends IStep
    case class CallStep(borrowCallable: Boolean, args: List[IExpressionPE]) extends IStep
    case class IndexStep(args: List[IExpressionPE]) extends IStep
    def step: Parser[IStep] = {
      def afterDot = {
        (integer ^^ { case IntLiteralPE(range, value) => LookupPE(StringP(range, value.toString), None) }) |
        templateSpecifiedLookup |
        lookup
      }

      (opt("^") ~ ("." ~> optWhite ~> afterDot) ~ opt(optWhite ~> packExpr) ^^ {
        case moveContainer ~ lookup ~ None => {
          vassert(moveContainer.isEmpty)
          MemberAccessStep(lookup)
        }
        case moveContainer ~ name ~ Some(args) => {
          MethodCallStep(moveContainer.isEmpty, name, args)
        }
      }) |
      (opt("^") ~ packExpr ^^ {
        case moveContainer ~ pack => CallStep(moveContainer.isEmpty, pack)
      }) |
      (indexExpr ^^ IndexStep)
    }

    // The float is up here because we don't want the 0.0 in [[4]].0.0 to be parsed as a float.
    float |
    // We dont have the optWhite here because we dont want to allow spaces before calls.
    // We dont want to allow moo (4) because we want each statements like this:
    //   each moo (x){ println(x); }
    (pos ~ expressionElementLevel1 ~ rep(/*SEE ABOVE optWhite ~> */step) ~ pos ^^ {
      case begin ~ first ~ restWithDots ~ end => {
        restWithDots.foldLeft(first)({
          case (prev, MethodCallStep(borrowContainer, lookup, args)) => {
            MethodCallPE(Range(begin, end), prev, borrowContainer, lookup, args)
          }
          case (prev, CallStep(borrowCallable, args)) => {
            FunctionCallPE(Range(begin, end), prev, args, borrowCallable)
          }
          case (prev, MemberAccessStep(name)) => {
            DotPE(Range(begin, end), prev, name)
          }
          case (prev, IndexStep(args)) => {
            DotCallPE(Range(begin, end), prev, args)
          }
//          // anExpr.4
//          case (prev, (None ~ Some(".") ~ (i @ IntLiteralPE(_, _)))) => DotCallPE(prev, PackPE(List(i)), borrowContainer = true)
//          // anExpr^.4
//          case (prev, (Some("^") ~ Some(".") ~ (i @ IntLiteralPE(_, _)))) => DotCallPE(prev, PackPE(List(i)), borrowContainer = false)
//          // anExpr(4)
//          case (prev, (None ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(Range(begin, end), prev, pack.elements, borrowCallable = true)
//          // anExpr^(4)
//          case (prev, (Some("^") ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(Range(begin, end), prev, pack.elements, borrowCallable = false)
//          // anExpr.(4)
//          case (prev, (None ~ Some(".") ~ (pack @ PackPE(_)))) => DotCallPE(prev, pack, borrowContainer = true)
//          // anExpr^.(4)
//          case (prev, (Some("^") ~ Some(".") ~ (pack @ PackPE(_)))) => DotCallPE(prev, pack, borrowContainer = false)
//          // anExpr.bork or anExpr.bork<Lork>, which only really makes sense when coming before a call like (4)
//          case (prev, (None ~ Some(".") ~ (lookup @ LookupPE(_, _)))) => DotPE(Range(begin, end), prev, lookup, borrowContainer = true)
//          // anExpr^.bork or anExpr^.bork<Lork>, which only really makes sense when coming before a call like (4)
//          case (prev, (Some("^") ~ Some(".") ~ (lookup @ LookupPE(_, _)))) => DotPE(Range(begin, end), prev, lookup, borrowContainer = false)
        })
      }
    })
  }

  // Parses expressions that can contain postfix operators, like function calling
  // or dots.
  private[parser] def postfixableExpressions: Parser[IExpressionPE] = {
    ret | mutate | ifLadder | lend | not | expressionLevel9
  }

  // Binariable = can have binary operators in it
  def binariableExpression[T](
      innerParser: Parser[IExpressionPE],
      binaryOperatorParser: Parser[T],
      combiner: (Range, T, IExpressionPE, IExpressionPE) => IExpressionPE): Parser[IExpressionPE] = {
    pos ~ innerParser ~ rep(binaryOperatorParser ~ innerParser ~ pos) ^^ {
      case begin ~ firstElement ~ restBinaryOperatorsAndElements => {
        val (_, resultExpr) =
          restBinaryOperatorsAndElements.foldLeft((begin, firstElement))({
            case ((previousResultBegin, previousResult), (operator ~ nextElement ~ nextElementEnd)) => {
              val range = Range(previousResultBegin, nextElementEnd)
              val combinedExpr = combiner(range, operator, previousResult, nextElement)
              (nextElementEnd, combinedExpr)
            }
          })
        resultExpr
      }
    }
  }

  def expression: Parser[IExpressionPE] = {
    // These extra white parsers are here otherwise we stop early when we're just looking for "<", in "<=".
    // Expecting the whitespace means we parse the entire operator.

    val withMultDiv =
      binariableExpression(
        postfixableExpressions,
        white ~> (pstr("*") | pstr("/")) <~ white,
        (range, op: StringP, left, right) => FunctionCallPE(range, LookupPE(op, None), List(left, right), true))

    val withAddSubtract =
      binariableExpression(
        withMultDiv,
        white ~> (pstr("+") | pstr("-")) <~ white,
        (range, op: StringP, left, right) => FunctionCallPE(range, LookupPE(op, None), List(left, right), true))

    val withComparisons =
      binariableExpression(
        withAddSubtract,
        white ~> specialOperators <~ white,
        (range, op: StringP, left, right) => FunctionCallPE(range, LookupPE(op, None), List(left, right), true))

//    val withAnd =
//      binariableExpression(
//        withAddSubtract,
//        "and".r,
//        (_: String, left, right) => AndPE(left, right))
//
//    val withOr =
//      binariableExpression(
//        withAnd,
//        "or".r,
//        (_: String, left, right) => OrPE(left, right))

    val withCustomBinaries =
      binariableExpression(
        withComparisons,
        not(white ~> pstr("=") <~ white) ~> (white ~> infixFunctionIdentifier <~ white),
        (range, funcName: StringP, left, right) => FunctionCallPE(range, LookupPE(funcName, None), List(left, right), true))

    withCustomBinaries |
    (specialOperators ^^ (op => LookupPE(op, None)))
  }

  sealed trait StatementType
  case object FunctionReturnStatementType extends StatementType
  case object BlockReturnStatementType extends StatementType
  case object NormalResultStatementType extends StatementType

  // The boolean means it's a result or a return, it should be the last thing in the block.
  def statementOrResult: Parser[(IExpressionPE, Boolean)] = {
    pos ~ (opt(("="|"ret") <~ optWhite) ~ statement) ~ pos ^^ {
      case begin ~ (maybeResult ~ expr) ~ end => {
        (maybeResult, expr) match {
          case (None, expr) => (expr, false)
          case (Some("="), expr) => (expr, true)
          case (Some("ret"), expr) => (ReturnPE(Range(begin, end), expr), true)
        }
      }
    }
  }

  def multiStatementBlock: Parser[List[IExpressionPE]] = {
    // Can't just do a rep(statement <~ optWhite) ~ ("=" ~> optWhite ~> statement) because
    // "= doThings(a);" is actually a valid statement, which looks like doThings(=, a);
    rep1sep(statementOrResult, optWhite) ~ pos ^^ {
      case statements ~ end => {
        statements match {
          case List() => List(VoidPE(Range(end, end)))
          case resultsOrStatements => {
            if (resultsOrStatements.init.exists(_._2)) {
              vfail("wot")
            }
            val exprs =
              if (resultsOrStatements.last._2) {
                resultsOrStatements.map(_._1)
              } else {
                resultsOrStatements.map(_._1) :+ VoidPE(Range(end, end))
              }
            exprs
          }
        }
      }
    }
  }

  def blockExprs: Parser[List[IExpressionPE]] = {
    multiStatementBlock |
      ((expression) ^^ { case e => List(e) }) |
      (pos ^^ { case p => List(VoidPE(Range(p, p)))})
  }

  private[parser] def tupleExpr: Parser[IExpressionPE] = {
    pos ~ ("[" ~> optWhite ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]") ~ pos ^^ {
      case begin ~ (a:List[IExpressionPE]) ~ end => {
        SequencePE(Range(begin, end), a)
      }
    }
  }

  private[parser] def packExpr: Parser[List[IExpressionPE]] = {
    "(" ~> optWhite ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")"
  }

  private[parser] def indexExpr: Parser[List[IExpressionPE]] = {
    "[" ~> optWhite ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]"
  }

  private[parser] def filledParamLambda: Parser[FunctionP] = {
    pos ~ (patternPrototypeParams <~ optWhite) ~ opt(":" ~> optWhite ~> patternTemplex <~ optWhite) ~ bracedBlock ~ pos ^^ {
      case begin ~ patternParams ~ maybeReturn ~ body ~ end =>
        FunctionP(
          Range(begin, end), None, None, None, None, None, Some(patternParams), maybeReturn, Some(body))
    }
  }

  private[parser] def emptyParamLambda: Parser[FunctionP] = {
    pos ~ (patternPrototypeParams <~ optWhite) ~ opt(":" ~> optWhite ~> patternTemplex <~ optWhite) ~ (pos <~ "{" <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ patternParams ~ maybeReturn ~ bodyBegin ~ end =>
        FunctionP(
          Range(begin, end), None, None, None, None, None, Some(patternParams), maybeReturn,
          Some(BlockPE(Range(bodyBegin, end), List(VoidPE(Range(end, end))))))
    }
  }

  private[parser] def filledParamLessLambda: Parser[FunctionP] = {
    pos ~ bracedBlock ~ pos ^^ {
      case begin ~ body ~ end => FunctionP(Range(begin, end), None, None, None, None, None, None, None, Some(body))
    }
  }

  private[parser] def emptyParamLessLambda: Parser[FunctionP] = {
    (pos <~ "{" <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ end =>
      FunctionP(
        Range(begin, end), None, None, None, None, None, None, None,
        Some(BlockPE(Range(begin, end), List(VoidPE(Range(end, end))))))
    }
  }

  private[parser] def lambda: Parser[LambdaPE] = {
    (filledParamLambda | emptyParamLambda | filledParamLessLambda | emptyParamLessLambda) ^^ LambdaPE
  }

  private[parser] def patternPrototypeParam: Parser[PatternPP] = {
    atomPattern ^^ {
      case pattern => pattern
    }
  }

  def patternPrototypeParams: Parser[ParamsP] = {
    pos ~ ("(" ~> optWhite ~> repsep(optWhite ~> patternPrototypeParam, optWhite ~> ",") <~ optWhite <~ ")") ~ pos ^^ {
      case begin ~ params ~ end => ParamsP(Range(begin, end), params)
    }
  }
}
