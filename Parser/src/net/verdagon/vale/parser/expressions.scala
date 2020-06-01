package net.verdagon.vale.parser

import net.verdagon.vale.vassert

import scala.util.parsing.input.{Position, Positional}

trait IExpressionPE

case class VoidPE() extends IExpressionPE {}

case class LendPE(expr: IExpressionPE) extends IExpressionPE

case class AndPE(left: IExpressionPE, right: IExpressionPE) extends IExpressionPE

case class OrPE(left: IExpressionPE, right: IExpressionPE) extends IExpressionPE

case class MutablePE(expr: IExpressionPE) extends IExpressionPE

case class IfPE(range: Range, condition: BlockPE, thenBody: BlockPE, elseBody: BlockPE) extends IExpressionPE
case class WhilePE(condition: BlockPE, body: BlockPE) extends IExpressionPE
case class MutatePE(mutatee: IExpressionPE, expr: IExpressionPE) extends IExpressionPE
case class ReturnPE(expr: IExpressionPE) extends IExpressionPE
case class SwapPE(exprA: IExpressionPE, exprB: IExpressionPE) extends IExpressionPE

case class LetPE(
  range: Range,
    templateRules: List[IRulexPR],
    pattern: PatternPP,
    expr: IExpressionPE
) extends IExpressionPE

case class RepeaterBlockPE(expression: IExpressionPE) extends IExpressionPE

case class RepeaterBlockIteratorPE(expression: IExpressionPE) extends IExpressionPE
case class PackPE(elements: List[IExpressionPE]) extends IExpressionPE
case class SequencePE(elements: List[IExpressionPE]) extends IExpressionPE

case class RepeaterPackPE(expression: IExpressionPE) extends IExpressionPE
case class RepeaterPackIteratorPE(expression: IExpressionPE) extends IExpressionPE

case class IntLiteralPE(range: Range, value: Int) extends IExpressionPE
case class BoolLiteralPE(value: Boolean) extends IExpressionPE
case class StrLiteralPE(value: StringP) extends IExpressionPE
case class FloatLiteralPE(value: Float) extends IExpressionPE

case class DotPE(left: IExpressionPE, member: LookupPE, borrowContainer: Boolean) extends IExpressionPE

case class DotCallPE(left: IExpressionPE, indexExpr: PackPE, borrowContainer: Boolean) extends IExpressionPE

case class FunctionCallPE(
  callableExpr: IExpressionPE,
  argExprs: List[IExpressionPE],
  borrowCallable: Boolean
) extends IExpressionPE

//case class MethodCall0(callableExpr: Expression0, objectExpr: Expression0, argsExpr: Pack0) extends Expression0

case class LookupPE(name: StringP, templateArgs: List[ITemplexPT]) extends IExpressionPE
case class MagicParamLookupPE(range: Range) extends IExpressionPE

case class LambdaPE(function: FunctionP) extends IExpressionPE

case class BlockPE(range: Range, elements: List[IExpressionPE]) extends IExpressionPE {
  // Every element should have at least one expression, because a block will
  // return the last expression's result as its result.
  // Even empty blocks aren't empty, they have a void() at the end.
  vassert(elements.size >= 1)
}
