package net.verdagon.vale.scout

import net.verdagon.vale.parser.{MutabilityP, VariabilityP}
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules.{IRulexSR, ITypeSR}
import net.verdagon.vale.vassert

// patternId is a unique number, can be used to make temporary variables that wont
// collide with other things
case class LetSE(
    patternId: Int,
    rules: List[IRulexSR],
    allRunes: Set[String],
    pattern: AtomSP,
    expr: IExpressionSE) extends IExpressionSE

case class IfSE(condition: BlockSE, thenBody: BlockSE, elseBody: BlockSE) extends IExpressionSE

case class WhileSE(condition: BlockSE, body: BlockSE) extends IExpressionSE

case class ExprMutateSE(mutatee: IExpressionSE, expr: IExpressionSE) extends IExpressionSE
case class GlobalMutateSE(name: String, expr: IExpressionSE) extends IExpressionSE
case class LocalMutateSE(name: String, expr: IExpressionSE) extends IExpressionSE

case class ExpressionLendSE(innerExpr1: IExpressionSE) extends IExpressionSE


//case class CurriedFunc3(closureExpr: Expression3, funcName: String) extends Expression3

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use


sealed trait IVariableUseCertainty
case object Used extends IVariableUseCertainty
case object NotUsed extends IVariableUseCertainty
case object MaybeUsed extends IVariableUseCertainty

case class LocalVariable1(
    varName: String,
    variability: VariabilityP,
    selfBorrowed: IVariableUseCertainty,
    selfMoved: IVariableUseCertainty,
    selfMutated: IVariableUseCertainty,
    childBorrowed: IVariableUseCertainty,
    childMoved: IVariableUseCertainty,
    childMutated: IVariableUseCertainty)

case class BodySE(
    // These are all the variables we use from parent environments.
    // We have these so templar doesn't have to dive through all the functions
    // that it calls (impossible) to figure out what's needed in a closure struct.
    closuredNames: Set[String],

    block: BlockSE
)// extends IExpressionSE

case class BlockSE(
  // This shouldn't be ordered yet because we introduce new locals all the
  // time in templar, easier to just order them in hammer.
  locals: Set[LocalVariable1],

  exprs: List[IExpressionSE],
) extends IExpressionSE {
  // Every element should have at least one expression, because a block will
  // return the last expression's result as its result.
  // Even empty blocks aren't empty, they have a void() at the end.
  vassert(exprs.size >= 1)
}

case class ArgLookupSE(index: Int) extends IExpressionSE

case class CheckRefCountSE(
    refExpr: IExpressionSE,
    category: RefCountCategory,
    numExpr: IExpressionSE) extends IExpressionSE

 // These things will be separated by semicolons, and all be joined in a block
case class RepeaterBlockSE(expression: IExpressionSE) extends IExpressionSE

// Results in a pack, represents the differences between the expressions
case class RepeaterBlockIteratorSE(expression: IExpressionSE) extends IExpressionSE

case class PackSE(elements: List[IExpressionSE]) extends IExpressionSE
case class ReturnSE(inner: IExpressionSE) extends IExpressionSE
case class VoidSE() extends IExpressionSE {}

case class SequenceESE(elements: List[IExpressionSE]) extends IExpressionSE

// This thing will be repeated, separated by commas, and all be joined in a pack
case class RepeaterPackSE(expression: IExpressionSE) extends IExpressionSE

// Results in a pack, represents the differences between the elements
case class RepeaterPackIteratorSE(expression: IExpressionSE) extends IExpressionSE

case class IntLiteralSE(value: Int) extends IExpressionSE

case class BoolLiteralSE(value: Boolean) extends IExpressionSE

case class StrLiteralSE(value: String) extends IExpressionSE

case class FloatLiteralSE(value: Float) extends IExpressionSE

case class FunctionSE(function: FunctionS) extends IExpressionSE

case class DotSE(left: IExpressionSE, member: String, borrowContainer: Boolean) extends IExpressionSE

case class DotCallSE(left: IExpressionSE, indexExpr: IExpressionSE) extends IExpressionSE

case class FunctionCallSE(callableExpr: IExpressionSE, argsPackExpr1: PackSE) extends IExpressionSE

//case class MethodCall0(callableExpr: Expression0, objectExpr: Expression0, argsExpr: Pack0) extends Expression0

case class TemplateSpecifiedLookupSE(name: String, templateArgs: List[ITemplexS]) extends IExpressionSE

case class LocalLoadSE(name: String, borrow: Boolean) extends IExpressionSE
case class GlobalLoadSE(name: String) extends IExpressionSE

case class UnletSE(name: String) extends IExpressionSE


//case class Scramble0(elements: List[Expression0]) extends Expression0 {
//  vassert(!elements.isEmpty, "Can't have an empty scramble")
//}
//case class Scramble1(elements: List[Expression1]) extends Expression1 {
//  vassert(!elements.isEmpty, "Can't have an empty scramble")
//}