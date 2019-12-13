package net.verdagon.vale.astronomer

import net.verdagon.vale.parser.{MutabilityP, VariabilityP}
import net.verdagon.vale.scout.{ITemplexS, LocalVariable1, RefCountCategory}
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules.IRulexSR
import net.verdagon.vale.vassert

// patternId is a unique number, can be used to make temporary variables that wont
// collide with other things
case class LetAE(
    patternId: Int,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    pattern: AtomSP,
    expr: IExpressionAE) extends IExpressionAE

case class IfAE(condition: BlockAE, thenBody: BlockAE, elseBody: BlockAE) extends IExpressionAE

case class WhileAE(condition: BlockAE, body: BlockAE) extends IExpressionAE

case class ExprMutateAE(mutatee: IExpressionAE, expr: IExpressionAE) extends IExpressionAE
case class GlobalMutateAE(name: String, expr: IExpressionAE) extends IExpressionAE
case class LocalMutateAE(name: String, expr: IExpressionAE) extends IExpressionAE

case class ExpressionLendAE(innerExpr1: IExpressionAE) extends IExpressionAE
case class ReturnAE(innerExpr1: IExpressionAE) extends IExpressionAE


//case class CurriedFunc3(closureExpr: Expression3, funcName: String) extends Expression3

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use

//
//sealed trait IVariableUseCertainty
//case object Used extends IVariableUseCertainty
//case object NotUsed extends IVariableUseCertainty
//case object MaybeUsed extends IVariableUseCertainty

case class BodyAE(
    // These are all the variables we use from parent environments.
    // We have these so templar doesn't have to dive through all the functions
    // that it calls (impossible) to figure out what's needed in a closure struct.
    closuredNames: Set[String],

    block: BlockAE
) extends IExpressionAE

case class BlockAE(
  // This shouldn't be ordered yet because we introduce new locals all the
  // time in templar, easier to just order them in hammer.
  locals: Set[LocalVariable1],

  exprs: List[IExpressionAE],
) extends IExpressionAE {
  // Every element should have at least one expression, because a block will
  // return the last expression's result as its result.
  // Even empty blocks aren't empty, they have a void() at the end.
  vassert(exprs.size >= 1)
}

case class ConstructAE(
  tyype: ITemplexA,
  args: List[IExpressionAE]) extends IExpressionAE

case class ConstructArrayAE(
    typeTemplex: ITemplexA,
    sizeExpr: IExpressionAE,
    generatorExpr: IExpressionAE,
    mutability: MutabilityP) extends IExpressionAE

case class ArgLookupAE(index: Int) extends IExpressionAE

case class CheckRefCountAE(
    refExpr: IExpressionAE,
    category: RefCountCategory,
    numExpr: IExpressionAE) extends IExpressionAE

 // These things will be separated by semicolons, and all be joined in a block
case class RepeaterBlockAE(expression: IExpressionAE) extends IExpressionAE

// Results in a pack, represents the differences between the expressions
case class RepeaterBlockIteratorAE(expression: IExpressionAE) extends IExpressionAE

case class PackAE(elements: List[IExpressionAE]) extends IExpressionAE
case class VoidAE() extends IExpressionAE {}

case class SequenceEAE(elements: List[IExpressionAE]) extends IExpressionAE

// This thing will be repeated, separated by commas, and all be joined in a pack
case class RepeaterPackAE(expression: IExpressionAE) extends IExpressionAE

// Results in a pack, represents the differences between the elements
case class RepeaterPackIteratorAE(expression: IExpressionAE) extends IExpressionAE

case class IntLiteralAE(value: Int) extends IExpressionAE

case class BoolLiteralAE(value: Boolean) extends IExpressionAE

case class StrLiteralAE(value: String) extends IExpressionAE

case class FloatLiteralAE(value: Float) extends IExpressionAE

case class FunctionAE(function: FunctionA) extends IExpressionAE

case class DotAE(left: IExpressionAE, member: String, borrowContainer: Boolean) extends IExpressionAE

case class DotCallAE(left: IExpressionAE, indexExpr: IExpressionAE) extends IExpressionAE

case class FunctionCallAE(callableExpr: IExpressionAE, argsPackExpr1: PackAE) extends IExpressionAE

//case class MethodCall0(callableExpr: Expression0, objectExpr: Expression0, argsExpr: Pack0) extends Expression0

case class TemplateSpecifiedLookupAE(name: String, templateArgs: List[ITemplexS]) extends IExpressionAE

case class LocalLoadAE(name: String, borrow: Boolean) extends IExpressionAE
case class GlobalLoadAE(name: String) extends IExpressionAE

case class UnletAE(name: String) extends IExpressionAE

case class ArrayLengthAE(arrayExpr: IExpressionAE) extends IExpressionAE
