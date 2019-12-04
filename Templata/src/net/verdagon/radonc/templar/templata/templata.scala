package net.verdagon.radonc.templar.templata

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List


sealed trait ITemplata extends Queriable2 {
  def order: Int;
  def tyype: ITemplataType
}

case class CoordTemplata(reference: Coord) extends ITemplata {
  override def order: Int = 1;
  override def tyype: ITemplataType = CoordTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
case class KindTemplata(referend: Kind) extends ITemplata {
  override def order: Int = 2;
  override def tyype: ITemplataType = KindTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ referend.all(func)
  }
}
case class ArrayTemplateTemplata() extends ITemplata {
  override def order: Int = 3;
  override def tyype: ITemplataType = TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class FunctionTemplata(
  outerEnv: IEnvironment, // has the name of the surrounding environment, does NOT include function's name.
  function: FunctionA
) extends ITemplata {
  override def order: Int = 6
  override def tyype: ITemplataType = vfail()

  // Make sure we didn't accidentally code something to include the function's name as
  // the last step.
  // This assertion is helpful now, but will false-positive trip when someone
  // tries to make an interface with the same name as its containing. At that point,
  // feel free to remove this assertion.
  vassert(!outerEnv.fullName.steps.lastOption.map(_.humanName).contains(function.name))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class StructTemplata(
  env: NamespaceEnvironment, // has the name of the surrounding environment, does NOT include struct's name.
  originStruct: StructA,
) extends ITemplata {
  override def order: Int = 7
  override def tyype: ITemplataType = originStruct.tyype

  // Make sure we didn't accidentally code something to include the struct's name as
  // the last step.
  // This assertion is helpful now, but will false-positive trip when someone
  // tries to make an interface with the same name as its containing. At that point,
  // feel free to remove this assertion.
  vassert(!env.fullName.steps.lastOption.map(_.humanName).contains(originStruct.name))


  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class InterfaceTemplata(
  env: NamespaceEnvironment, // has the name of the surrounding environment, does NOT include interface's name.
  originInterface: InterfaceA
) extends ITemplata {
  override def order: Int = 8
  override def tyype: ITemplataType = originInterface.tyype

  // Make sure we didn't accidentally code something to include the interface's name as
  // the last step.
  // This assertion is helpful now, but will false-positive trip when someone
  // tries to make an interface with the same name as its containing. At that point,
  // feel free to remove this assertion.
  vassert(!env.fullName.steps.lastOption.map(_.humanName).contains(originInterface.name))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class ImplTemplata(
  env: IEnvironment,
  impl: ImplA
) extends ITemplata {
  override def order: Int = 9
  override def tyype: ITemplataType = vfail()

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class ExternFunctionTemplata(header: FunctionHeader2) extends ITemplata {
  override def order: Int = 1337
  override def tyype: ITemplataType = vfail()

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ header.all(func)
  }
}

case class OwnershipTemplata(ownership: Ownership) extends ITemplata {
  override def order: Int = 10;
  override def tyype: ITemplataType = OwnershipTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ ownership.all(func)
  }
}
case class VariabilityTemplata(variability: Variability) extends ITemplata {
  override def order: Int = 11;
  override def tyype: ITemplataType = VariabilityTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ variability.all(func)
  }
}
case class MutabilityTemplata(mutability: Mutability) extends ITemplata {
  override def order: Int = 12;
  override def tyype: ITemplataType = MutabilityTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ mutability.all(func)
  }
}
case class PermissionTemplata(mutability: Permission) extends ITemplata {
  override def order: Int = 13;
  override def tyype: ITemplataType = PermissionTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ mutability.all(func)
  }
}
case class LocationTemplata(mutability: Location) extends ITemplata {
  override def order: Int = 14;
  override def tyype: ITemplataType = LocationTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ mutability.all(func)
  }
}

case class BooleanTemplata(value: Boolean) extends ITemplata {
  override def order: Int = 15;
  override def tyype: ITemplataType = BooleanTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class IntegerTemplata(value: Integer) extends ITemplata {
  override def order: Int = 16;
  override def tyype: ITemplataType = IntegerTemplataType

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
