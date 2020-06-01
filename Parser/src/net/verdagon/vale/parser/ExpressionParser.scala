package net.verdagon.vale.parser

import net.verdagon.vale.parser.patterns.PatternParser
import net.verdagon.vale.vfail

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
    (exprIdentifier ^^ (i => LookupPE(i, List())))
  }

  private[parser] def templateSpecifiedLookup: Parser[LookupPE] = {
    (exprIdentifier <~ optWhite) ~ ("<" ~> optWhite ~> repsep(templex, optWhite ~> "," <~ optWhite) <~ optWhite <~ ">") ^^ {
      case name ~ templateArgs => LookupPE(name, templateArgs)
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
    (pstr("not") <~ optWhite) ~ postfixableExpressions ^^ {
      case not ~ expr => FunctionCallPE(LookupPE(not, List()), List(expr), true)
    }
  }

  private[parser] def ret: Parser[IExpressionPE] = {
    "ret" ~> optWhite ~> expression ^^ {
      case sourceExpr => ReturnPE(sourceExpr)
    }
  }

  private[parser] def mutate: Parser[IExpressionPE] = {
    ("mut" ~> white ~> expression <~ white <~ "=" <~ white) ~ expression ^^ {
      case destinationExpr ~ sourceExpr => MutatePE(destinationExpr, sourceExpr)
    }
  }

  private[parser] def swap: Parser[IExpressionPE] = {
    ("exch" ~> optWhite ~> (expression <~ optWhite <~ "," <~ optWhite) ~ (expression <~ optWhite)) ^^ {
      case leftExpr ~ rightExpr => SwapPE(leftExpr, rightExpr)
    }
  }

  private[parser] def bracedBlock = ("{" ~> optWhite ~> block <~ optWhite <~ "}")

  private[parser] def eachI: Parser[FunctionCallPE] = {
    pstr("eachI") ~ (white ~> postfixableExpressions <~ white) ~ lambda ^^ {
      case eachI ~ collection ~ lam => FunctionCallPE(LookupPE(eachI, List()), List(collection, lam), true)
    }
  }

  private[parser] def whiile: Parser[WhilePE] = {
    ("while" ~> optWhite ~> "(" ~> optWhite ~> block <~ optWhite <~ ")" <~ optWhite) ~ bracedBlock ^^ {
      case cond ~ thenBlock => WhilePE(cond, thenBlock)
    }
  }

  private[parser] def ifPart: Parser[(BlockPE, BlockPE)] = {
    ("if" ~> optWhite ~> "(" ~> optWhite ~> block <~ optWhite <~ ")" <~ optWhite) ~ bracedBlock ^^ {
      case condLambda ~ thenLambda => (condLambda, thenLambda)
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
            case None => BlockPE(Range(end, end), List(VoidPE()))
            case Some(block) => block
          }
        val rootElseBlock =
          ifElses.foldRight(finalElse)({
            case ((conditionExpr, thenBlock), elseBlock) => {
              BlockPE(
                Range(conditionExpr.range.begin, thenBlock.range.end),
                List(
                  IfPE(
                    Range(conditionExpr.range.begin, thenBlock.range.end),
                    conditionExpr, thenBlock, elseBlock)))
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
    (mutate <~ ";") | swap | let | whiile | eachI | ifLadder | (expression <~ ";") | ("block" ~> optWhite ~> bracedBlock)
  }

  private[parser] def expressionElementLevel1: Parser[IExpressionPE] = {
    stringExpr |
      integer |
      ("true" ^^^ BoolLiteralPE(true)) |
      ("false" ^^^ BoolLiteralPE(false)) |
      lambda |
      packExpr |
      tupleExpr |
      templateSpecifiedLookup |
      (lookup ^^ {
        case l @ LookupPE(StringP(r, "_"), List()) => MagicParamLookupPE(r)
        case other => other
      })
  }

  private[parser] def expressionLevel9: Parser[IExpressionPE] = {
    // The float is up here because we don't want the 0.0 in [[4]].0.0 to be parsed as a float.
    float |
    expressionElementLevel1 ~ rep(opt("^") ~ opt(".") ~ expressionElementLevel1) ^^ {
      case first ~ restWithDots => {
        restWithDots.foldLeft(first)({
          // anExpr.4
          case (prev, (None ~ Some(".") ~ (i @ IntLiteralPE(_)))) => DotCallPE(prev, PackPE(List(i)), borrowContainer = true)
          // anExpr^.4
          case (prev, (Some("^") ~ Some(".") ~ (i @ IntLiteralPE(_)))) => DotCallPE(prev, PackPE(List(i)), borrowContainer = false)
          // anExpr(4)
          case (prev, (None ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(prev, pack.elements, borrowCallable = true)
          // anExpr^(4)
          case (prev, (Some("^") ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(prev, pack.elements, borrowCallable = false)
          // anExpr.(4)
          case (prev, (None ~ Some(".") ~ (pack @ PackPE(_)))) => DotCallPE(prev, pack, borrowContainer = true)
          // anExpr^.(4)
          case (prev, (Some("^") ~ Some(".") ~ (pack @ PackPE(_)))) => DotCallPE(prev, pack, borrowContainer = false)
          // anExpr.bork or anExpr.bork<Lork>, which only really makes sense when coming before a call like (4)
          case (prev, (None ~ Some(".") ~ (lookup @ LookupPE(_, _)))) => DotPE(prev, lookup, borrowContainer = true)
          // anExpr^.bork or anExpr^.bork<Lork>, which only really makes sense when coming before a call like (4)
          case (prev, (Some("^") ~ Some(".") ~ (lookup @ LookupPE(_, _)))) => DotPE(prev, lookup, borrowContainer = false)
        })
      }
    }
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
      combiner: (T, IExpressionPE, IExpressionPE) => IExpressionPE): Parser[IExpressionPE] = {
    innerParser ~ rep((binaryOperatorParser) ~ innerParser) ^^ {
      case firstElement ~ restBinaryOperatorsAndElements => {
        restBinaryOperatorsAndElements.foldLeft(firstElement)({
          case (previousResult, (operator ~ nextElement)) => {
            combiner(operator, previousResult, nextElement)
          }
        })
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
        (op: StringP, left, right) => FunctionCallPE(LookupPE(op, List()), List(left, right), true))

    val withAddSubtract =
      binariableExpression(
        withMultDiv,
        white ~> (pstr("+") | pstr("-")) <~ white,
        (op: StringP, left, right) => FunctionCallPE(LookupPE(op, List()), List(left, right), true))

    val withComparisons =
      binariableExpression(
        withAddSubtract,
        white ~> specialOperators <~ white,
        (op: StringP, left, right) => FunctionCallPE(LookupPE(op, List()), List(left, right), true))

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
        (funcName: StringP, left, right) => FunctionCallPE(LookupPE(funcName, List()), List(left, right), true))

    withCustomBinaries |
    (specialOperators ^^ (op => LookupPE(op, List())))
  }

  sealed trait StatementType
  case object FunctionReturnStatementType extends StatementType
  case object BlockReturnStatementType extends StatementType
  case object NormalResultStatementType extends StatementType

  // The boolean means it's a result or a return, it should be the last thing in the block.
  def statementOrResult: Parser[(IExpressionPE, Boolean)] = {
    (opt(("="|"ret") <~ optWhite) ~ statement) ^^ {
      case None ~ expr => (expr, false)
      case Some("=") ~ expr => (expr, true)
      case Some("ret") ~ expr => (ReturnPE(expr), true)
    }
  }

  def multiStatementBlock = {
    // Can't just do a rep(statement <~ optWhite) ~ ("=" ~> optWhite ~> statement) because
    // "= doThings(a);" is actually a valid statement, which looks like doThings(=, a);
    pos ~ rep1sep(statementOrResult, optWhite) ~ pos ^^ {
      case begin ~ statements ~ end => {
        statements match {
          case List() => BlockPE(Range(begin, end), List(VoidPE()))
          case resultsOrStatements => {
            if (resultsOrStatements.init.exists(_._2)) {
              vfail("wot")
            }
            val exprs =
              if (resultsOrStatements.last._2) {
                resultsOrStatements.map(_._1)
              } else {
                resultsOrStatements.map(_._1) :+ VoidPE()
              }
            BlockPE(Range(begin, end), exprs)
          }
        }
      }
    }
  }

  def block: Parser[BlockPE] = {
    multiStatementBlock |
      ((pos ~ expression ~ pos) ^^ {
        case begin ~ e ~ end => BlockPE(Range(begin, end), List(e))
      }) |
      (pos ^^ { case p => BlockPE(Range(p, p), List(VoidPE()))})
  }

  private[parser] def tupleExpr: Parser[IExpressionPE] = {
    "[" ~> optWhite ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ optWhite <~ "]" ^^ {
      (a:List[IExpressionPE]) => {
        SequencePE(a)
      }
    }
  }

  private[parser] def packExpr: Parser[PackPE] = {
    "(" ~> optWhite ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")" ^^ {
      a => {
        PackPE(a)
      }
    }
  }

  private[parser] def filledParamLambda: Parser[FunctionP] = {
    pos ~ (patternPrototypeParams <~ optWhite) ~ opt(":" ~> optWhite ~> patternTemplex <~ optWhite) ~ ("{" ~> optWhite ~> block <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ patternParams ~ maybeReturn ~ maybeBody ~ end =>
        FunctionP(Range(begin, end), None, None, None, None, None, Some(patternParams), maybeReturn, Some(maybeBody))
    }
  }

  private[parser] def emptyParamLambda: Parser[FunctionP] = {
    pos ~ (patternPrototypeParams <~ optWhite) ~ opt(":" ~> optWhite ~> patternTemplex <~ optWhite) ~ (pos <~ "{" <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ patternParams ~ maybeReturn ~ bodyBegin ~ end =>
        FunctionP(
          Range(begin, end), None, None, None, None, None, Some(patternParams), maybeReturn,
          Some(BlockPE(Range(bodyBegin, end), List(VoidPE()))))
    }
  }

  private[parser] def filledParamLessLambda: Parser[FunctionP] = {
    pos ~ ("{" ~> optWhite ~> block <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ body ~ end => FunctionP(Range(begin, end), None, None, None, None, None, None, None, Some(body))
    }
  }

  private[parser] def emptyParamLessLambda: Parser[FunctionP] = {
    (pos <~ "{" <~ optWhite <~ "}") ~ pos ^^ {
      case begin ~ end =>
      FunctionP(
        Range(begin, end), None, None, None, None, None, None, None,
        Some(BlockPE(Range(begin, end), List(VoidPE()))))
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
