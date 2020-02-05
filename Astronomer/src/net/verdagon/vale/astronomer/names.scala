package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.CodeLocationS

// An absolute name is one where we know *exactly* where it's defined; if parser and scout
// put their brains together they could know exactly where the thing is.
case class AbsoluteNameA[+T <: INameA](file: String, initSteps: List[INameA], last: T) {// extends IImpreciseNameA[T] {
def addStep[Y <: INameA](newLast: Y): AbsoluteNameA[Y] = AbsoluteNameA[Y](file, initSteps :+ last, newLast)
  def steps: List[INameA] = initSteps :+ last
  def init: AbsoluteNameA[INameA] = AbsoluteNameA[INameA](file, initSteps.init, initSteps.last)
}
// An imprecise name is one where we don't know exactly where the thing is defined.
// For example, in
//   fn main() {
//     doStuff("hello");
//   }
// we don't know exactly where doStuff was defined, that depends on what overload the
// typing stage decides.
case class ImpreciseNameA[+T <: IImpreciseNameStepA](init: List[IImpreciseNameStepA], last: T) {//extends IImpreciseNameS[T] {
def addStep[Y <: IImpreciseNameStepA](newLast: Y): ImpreciseNameA[Y] = ImpreciseNameA[Y](init :+ last, newLast)
}

sealed trait INameA
sealed trait IVarNameA extends INameA
sealed trait IFunctionDeclarationNameA extends INameA
case class LambdaNameA(codeLocation: CodeLocationS) extends IFunctionDeclarationNameA
case class FunctionNameA(name: String, codeLocation: CodeLocationS) extends IFunctionDeclarationNameA
case class TopLevelCitizenDeclarationNameA(name: String, codeLocation: CodeLocationS) extends INameA
case class LambdaStructNameA(codeLocation: CodeLocationS) extends INameA
case class ImplNameA(codeLocation: CodeLocationS) extends INameA
case class LetNameA(codeLocation: CodeLocationS) extends INameA
case class UnnamedLocalNameA(codeLocation: CodeLocationS) extends IVarNameA
case class ClosureParamNameA() extends IVarNameA
case class MagicParamNameA(magicParamNumber: Int) extends IVarNameA
case class CodeVarNameA(name: String) extends IVarNameA
sealed trait IRuneA extends INameA
case class CodeRuneA(name: String) extends IRuneA
case class ImplicitRuneA(name: Int) extends IRuneA
case class MemberRuneA(memberIndex: Int) extends IRuneA
case class MagicImplicitRuneA(magicParamIndex: Int) extends IRuneA
case class ReturnRuneA() extends IRuneA


sealed trait IImpreciseNameStepA
case class CodeTypeNameA(name: String) extends IImpreciseNameStepA
// When we're calling a function, we're addressing an overload set, not a specific function.
// If we want a specific function, we use TopLevelDeclarationNameS.
case class GlobalFunctionFamilyNameA(name: String) extends IImpreciseNameStepA
case class ImpreciseCodeVarNameA(name: String) extends IImpreciseNameStepA