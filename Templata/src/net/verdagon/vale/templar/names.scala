package net.verdagon.vale.templar

import net.verdagon.vale.scout.CodeLocationS
import net.verdagon.vale.templar.templata.{CodeLocation2, CoordTemplata, ITemplata, Queriable2}
import net.verdagon.vale.templar.types.Coord

// Scout's/Astronomer's name parts correspond to where they are in the source code,
// but Templar's correspond more to what namespaces and stamped functions / structs
// they're in. See TNAD.

case class FullName2[+T <: IName2](initSteps: List[IName2], last: T) extends Queriable2 {
  def addStep[Y <: IName2](newLast: Y): FullName2[Y] = FullName2[Y](initSteps :+ last, newLast)
  def steps: List[IName2] = {
    last match {
      case GlobalNamespaceName2() => initSteps
      case _ => initSteps :+ last
    }
  }
  def init: FullName2[IName2] = FullName2[IName2](initSteps.init, initSteps.last)

  def all[X](func: PartialFunction[Queriable2, X]): List[X] = {
    List(this).collect(func) ++ initSteps.flatMap(_.all(func)) ++ last.all(func)
  }
}
// not sure if we need imprecise names in templar
//// An imprecise name is one where we don't know exactly where the thing is defined.
//// For example, in
////   fn main() {
////     doStuff("hello");
////   }
//// we don't know exactly where doStuff was defined, that depends on what overload the
//// typing stage decides.
//case class ImpreciseName2[+T <: IImpreciseNameStep2](init: List[IImpreciseNameStep2], last: T) {//extends IImpreciseNameS[T] {
//  def addStep[Y <: IImpreciseNameStep2](newLast: Y): ImpreciseName2[Y] = ImpreciseName2[Y](init :+ last, newLast)
//}

sealed trait IName2 extends Queriable2 {
  def order: Int
}
sealed trait IFunctionName2 extends IName2 {
  def templateArgs: List[ITemplata]
  def parameters: List[Coord]
}
sealed trait ICitizenName2 extends IName2 {
  def templateArgs: List[ITemplata]
}
sealed trait IStructName2 extends ICitizenName2
case class ImplDeclareName2(codeLocation: CodeLocation2) extends IName2 { def order = 1; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) ++ codeLocation.all(func) } }
case class LetName2(codeLocation: CodeLocation2) extends IName2 { def order = 2; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) ++ codeLocation.all(func) } }

sealed trait IVarName2 extends IName2
case class TemplarBlockResultVarName2(num: Int) extends IVarName2 { def order = 18; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class TemplarFunctionResultVarName2() extends IVarName2 { def order = 19; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class TemplarTemporaryVarName2(num: Int) extends IVarName2 { def order = 20; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class TemplarPatternMemberName2(num: Int, memberIndex: Int) extends IVarName2 { def order = 23; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class TemplarPatternPackName2(num: Int) extends IVarName2 { def order = 23; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class UnnamedLocalName2(codeLocation: CodeLocation2) extends IVarName2 { def order = 3; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) ++ codeLocation.all(func) } }
case class ClosureParamName2() extends IVarName2 { def order = 4; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class MagicParamName2(codeLocation2: CodeLocation2) extends IVarName2 { def order = 5; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class CodeVarName2(name: String) extends IVarName2 { def order = 6; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
// We dont use CodeVarName2(0), CodeVarName2(1) etc because we dont want the user to address these members directly.
case class AnonymousSubstructMemberName2(index: Int) extends IVarName2 { def order = 24; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class PrimitiveName2(humanName: String) extends IName2 { def order = 26; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
// Only made in templar
case class GlobalNamespaceName2() extends IName2 { def order = 25; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }

// We use this one to look for impls, which are disambiguated by the above ImplDeclareName2
//case class ImplImpreciseName2() extends IName2 { def order = 22; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }

case class FunctionName2(
  humanName: String,
  templateArgs: List[ITemplata],
  parameters: List[Coord]
) extends IFunctionName2 {
  def order = 13;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func)) ++ parameters.flatMap(_.all(func))
  }
}
case class FunctionTemplateName2(
    humanName: String,
    codeLocation: CodeLocation2
) extends IName2 {
  def order = 31;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ codeLocation.all(func)
  }
}
case class LambdaTemplateName2(
  codeLocation: CodeLocation2
) extends IName2 {
  def order = 31;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ codeLocation.all(func)
  }
}
case class ConstructorName2(
  parameters: List[Coord]
) extends IFunctionName2 {
  def order = 21;
  def templateArgs: List[ITemplata] = List()
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
//// We have this and LambdaStructName2 both because sometimes lambdas dont come with
//// a struct, like if they capture nothing. When they do come with structs, theyll both
//// be in the name, this one after the LambdaStructName2 name.
//case class LambdaName2(
//  codeLocation: CodeLocation2,
//  templateArgs: List[ITemplata],
//  parameters: List[Coord]
//) extends IFunctionName2 {
//  def order = 14;
//  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
//    List(this).collect(func) ++ templateArgs.flatMap(_.all(func)) ++ parameters.flatMap(_.all(func))
//  }
//}
case class StructName2(
  humanName: String,
  templateArgs: List[ITemplata]
) extends IStructName2 {
  def order = 15;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func))
  }
}
case class TupleName2(
  members: List[Coord]
) extends IStructName2 {
  override def templateArgs: List[ITemplata] = members.map(CoordTemplata)
  def order = 16;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ members.flatMap(_.all(func))
  }
}
case class LambdaStructName2(
  codeLocation: CodeLocation2,
) extends IStructName2 {
  def templateArgs: List[ITemplata] = List()
  def order = 17;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.toList.flatMap(_.all(func))
  }
}
case class InterfaceTemplateName2(
  humanName: String,
  codeLocation: CodeLocation2
) extends IName2 {
  def order = 30;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ codeLocation.all(func)
  }
}
case class StructTemplateName2(
  humanName: String,
  codeLocation: CodeLocation2
) extends IName2 {
  def order = 30;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ codeLocation.all(func)
  }
}
case class InterfaceName2(
  humanName: String,
  templateArgs: List[ITemplata]
) extends ICitizenName2 {
  def order = 33;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.toList.flatMap(_.all(func))
  }
}
case class AnonymousSubstructName2(methodNames: List[FullName2[IFunctionName2]]) extends IStructName2 {
  def order = 27;
  def templateArgs: List[ITemplata] = List()
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.toList.flatMap(_.all(func))
  }
}
case class AnonymousSubstructConstructorName2() extends IFunctionName2 {
  def order = 27;
  def templateArgs: List[ITemplata] = List()
  def parameters: List[Coord] = List()
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.toList.flatMap(_.all(func))
  }
}
case class AnonymousSubstructImplName2() extends IName2 {
  def order = 29;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
// This one is probably only used by the templar, so we can have a way to
// figure out the closure struct for a certain environment.
case class EnvClosureName2() extends IName2 {
  def order = 32;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

// This is an IName2 because we put these into the environment.
// We don't just reuse INameA because there are some templar-specific ones.
sealed trait IRune2 extends IName2
case class CodeRune2(name: String) extends IRune2 { def order = 7; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class ImplicitRune2(name: Int) extends IRune2 { def order = 8; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class MemberRune2(memberIndex: Int) extends IRune2 { def order = 9; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class MagicImplicitRune2(codeLocation: CodeLocation2) extends IRune2 { def order = 10; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class ReturnRune2() extends IRune2 { def order = 11; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }
case class SolverKindRune2(paramRune: IRune2) extends IRune2 { def order = 12; def all[T](func: PartialFunction[Queriable2, T]): List[T] = { List(this).collect(func) } }

case class AnonymousSubstructParentInterfaceRune2() extends IRune2 {
  def order = 28;
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

//
//sealed trait IImpreciseNameStep2
//case class CodeTypeName2(name: String) extends IImpreciseNameStep2
//// When we're calling a function, we're addressing an overload set, not a specific function.
//// If we want a specific function, we use TopLevelDeclarationNameS.
//case class GlobalFunctionFamilyName2(name: String) extends IImpreciseNameStep2
//case class ImpreciseCodeVarName2(name: String) extends IImpreciseNameStep2
