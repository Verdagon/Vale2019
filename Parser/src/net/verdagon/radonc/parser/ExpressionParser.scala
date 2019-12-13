package net.verdagon.radonc.parser

import net.verdagon.radonc.parser.patterns.PatternParser
import net.verdagon.radonc.vfail

import scala.util.parsing.combinator.RegexParsers

trait ExpressionParser extends RegexParsers with ParserUtils {
  private[parser] def templex: Parser[ITemplexPT]
  private[parser] def templateRulesPR: Parser[List[IRulexPR]]
  private[parser] def atomPattern: Parser[PatternPP]
  private[parser] def patternTemplex: Parser[ITemplexPPT]

  private[parser] def lookup: Parser[LookupPE] = {
    exprIdentifier ^^ (i => LookupPE(i, List()))
  }

  private[parser] def templateSpecifiedLookup: Parser[LookupPE] = {
    (exprIdentifier <~ optWhite) ~ ("<" ~> optWhite ~> repsep(templex, optWhite ~> "," <~ optWhite) <~ optWhite <~ ">") ^^ {
      case name ~ templateArgs => LookupPE(name, templateArgs)
    }
  }

  private[parser] def let: Parser[LetPE] = {
    (opt(templateRulesPR) <~ optWhite) ~
        (atomPattern <~ white <~ "=" <~ white) ~
        (expression <~ optWhite <~ ";") ^^ {
      case maybeTemplateRules ~ pattern ~ expr => {
        // We just threw away the topLevelRunes because let statements cant have them.
        LetPE(maybeTemplateRules.getOrElse(List()), pattern, expr)
      }
    }
  }

  private[parser] def lend: Parser[IExpressionPE] = {
    "&" ~> optWhite ~> postfixableExpressions ^^ LendPE
  }

  private[parser] def not: Parser[IExpressionPE] = {
    "not" ~> optWhite ~> postfixableExpressions ^^ {
      case expr => FunctionCallPE(LookupPE("not", List()), PackPE(List(expr)), true)
    }
  }

  private[parser] def ret: Parser[IExpressionPE] = {
    "ret" ~> optWhite ~> expression ^^ {
      case sourceExpr => ReturnPE(sourceExpr)
    }
  }

  private[parser] def mutate: Parser[IExpressionPE] = {
    ("mut" ~> optWhite ~> "(" ~> optWhite ~> expression <~ optWhite <~ ")" <~ optWhite <~ "=" <~ optWhite) ~ expression ^^ {
      case destinationExpr ~ sourceExpr => MutatePE(destinationExpr, sourceExpr)
    }
  }

  private[parser] def swap: Parser[IExpressionPE] = {
    ("exch" ~> optWhite ~> (expression <~ optWhite <~ "," <~ optWhite) ~ (expression <~ optWhite)) ^^ {
      case leftExpr ~ rightExpr => SwapPE(leftExpr, rightExpr)
    }
  }

  private[parser] def bracedBlock = ("{" ~> optWhite ~> block <~ optWhite <~ "}")

  private[parser] def whiile: Parser[WhilePE] = {
    ("while" ~> optWhite ~> bracedBlock <~ optWhite) ~ bracedBlock ^^ {
      case cond ~ thenBlock => WhilePE(cond, thenBlock)
    }
  }

  private[parser] def ifPart: Parser[(BlockPE, BlockPE)] = {
    ("if" ~> optWhite ~> "{" ~> optWhite ~> block <~ optWhite <~ "}" <~ optWhite) ~ bracedBlock ^^ {
      case condLambda ~ thenLambda => (condLambda, thenLambda)
    }
  }

  def ifLadder: Parser[IExpressionPE] = {
    (ifPart) ~
        rep(optWhite ~> "else" ~> optWhite ~> ifPart) ~
        opt(optWhite ~> "else" ~> optWhite ~> bracedBlock) ^^ {
      case rootIf ~ ifElses ~ maybeElseBlock => {
        val finalElse: BlockPE =
          maybeElseBlock match {
            case None => BlockPE(List(VoidPE()))
            case Some(block) => block
          }
        val rootElseBlock =
          ifElses.foldRight(finalElse)({
            case ((conditionExpr, thenBlock), elseBlock) => {
              BlockPE(List(IfPE(conditionExpr, thenBlock, elseBlock)))
            }
          })
        val (rootConditionLambda, rootThenLambda) = rootIf
        IfPE(rootConditionLambda, rootThenLambda, rootElseBlock)
      }
    }
  }

  def statement: Parser[IExpressionPE] = {
    // bracedBlock is at the end because we want to be able to parse "{print(_)}(4);" as an expression.
    // debt: "block" is here temporarily because we get ambiguities in this case:
    //   fn main() { {_ + _}(4 + 5) }
    // because it mistakenly successfully parses {_ + _} then dies on the next part.
    swap | let | whiile | ifLadder | (expression <~ ";") | ("block" ~> optWhite ~> bracedBlock)
  }

  private[parser] def expressionElementLevel1: Parser[IExpressionPE] = {
    string |
      integer |
      ("true" ^^^ BoolLiteralPE(true)) |
      ("false" ^^^ BoolLiteralPE(false)) |
      lambda |
      packExpr |
      tupleExpr |
      templateSpecifiedLookup |
      lookup
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
          case (prev, (None ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(prev, pack, borrowCallable = true)
          // anExpr^(4)
          case (prev, (Some("^") ~ None ~ (pack @ PackPE(_)))) => FunctionCallPE(prev, pack, borrowCallable = false)
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

  def anyOf(strings: Set[String]): Parser[String] = {
    val x: Parser[String] = failure("wat")
    strings.foldLeft(x)({ case (a, b) => a | b })
  }

  // Binariable = can have binary operators in it
  // These are separated by `white ~> binaryOperatorParser <~ white` because otherwise
  // we stop early when we're just looking for "<", in "<=". Expecting the whitespace means
  // we parse the entire operator.
  def binariableExpression[T](
      innerParser: Parser[IExpressionPE],
      binaryOperatorParser: Parser[T],
      combiner: (T, IExpressionPE, IExpressionPE) => IExpressionPE): Parser[IExpressionPE] = {
    innerParser ~ rep((white ~> binaryOperatorParser <~ white) ~ innerParser) ^^ {
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
    val withMultDiv =
      binariableExpression(
        postfixableExpressions,
        anyOf(Set("*", "/")),
        (op: String, left, right) => FunctionCallPE(LookupPE(op, List()), PackPE(List(left, right)), true))

    val withAddSubtract =
      binariableExpression(
        withMultDiv,
        anyOf(Set("+", "-")),
        (op: String, left, right) => FunctionCallPE(LookupPE(op, List()), PackPE(List(left, right)), true))

    val withLessGreater =
      binariableExpression(
        withAddSubtract,
        anyOf(Set("<", ">")),
        (op: String, left, right) => FunctionCallPE(LookupPE(op, List()), PackPE(List(left, right)), true))

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
        withLessGreater,
        infixFunctionIdentifier,
        (funcName: String, left, right) => FunctionCallPE(LookupPE(funcName, List()), PackPE(List(left, right)), true))

    withCustomBinaries
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
    rep1sep(statementOrResult, optWhite) ^^ {
      case List() => BlockPE(List(VoidPE()))
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
        BlockPE(exprs)
      }
    }
  }

  def block: Parser[BlockPE] = {
    multiStatementBlock |
        (expression ^^ (e => BlockPE(List(e)))) |
        success(BlockPE(List(VoidPE())))
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

  private[parser] def filledParamLambda: Parser[LambdaPE] = {
    ("{" ~> patternPrototypeParams) ~ opt(patternTemplex) ~ (optWhite ~> block <~ optWhite <~ "}") ^^ {
      case patternParams ~ maybeReturn ~ maybeBody => LambdaPE(FunctionP(None, false, false, true, List(), List(), patternParams, maybeReturn, Some(maybeBody)))
    }
  }

  private[parser] def emptyParamLambda: Parser[LambdaPE] = {
    ("{" ~> patternPrototypeParams) ~ opt(patternTemplex) <~ optWhite <~ "}" ^^ {
      case patternParams ~ maybeReturn => LambdaPE(FunctionP(None, false, false, true, List(), List(), patternParams, maybeReturn, Some(BlockPE(List(VoidPE())))))
    }
  }

  private[parser] def filledParamLessLambda: Parser[LambdaPE] = {
    "{" ~> optWhite ~> block <~ optWhite <~ "}" ^^ {
      case b => LambdaPE(FunctionP(None, false, false, true, List(), List(), List(), None, Some(b)))
    }
  }

  private[parser] def emptyParamLessLambda: Parser[LambdaPE] = {
    "{" ~> optWhite <~ "}" ^^^ {
      LambdaPE(FunctionP(None, false, false, true, List(), List(), List(), None, Some(BlockPE(List(VoidPE())))))
    }
  }

  private[parser] def lambda: Parser[LambdaPE] = {
    filledParamLambda | emptyParamLambda | filledParamLessLambda | emptyParamLessLambda
  }

  private[parser] def patternPrototypeParam: Parser[PatternPP] = {
    atomPattern ^^ {
      case pattern => pattern
    }
  }

  def patternPrototypeParams: Parser[List[PatternPP]] = {
    "(" ~> optWhite ~> repsep(optWhite ~> patternPrototypeParam, optWhite ~> ",") <~ optWhite <~ ")"
  }

}
