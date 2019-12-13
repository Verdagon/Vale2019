package net.verdagon.vale.hammer

import net.verdagon.vale.scout.CodeLocation
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vfail

case class Reference3[+T <: Referend3](ownership: Ownership, kind: T) {
  kind match {
    case Int3() | Bool3() | Str3() | Float3() => {
      if (ownership != Share) {
        vfail("wot")
      }
    }
    case Void3() => {
      if (ownership != Raw) {
        vfail("wot")
      }
    }
    case _ =>
  }

  def expectKnownSizeArrayReference() = {
    kind match {
      case at3 @ KnownSizeArrayT3(_, _) => Reference3[KnownSizeArrayT3](ownership, at3)
    }
  }
  def expectUnknownSizeArrayReference() = {
    kind match {
      case at3 @ UnknownSizeArrayT3(_) => Reference3[UnknownSizeArrayT3](ownership, at3)
    }
  }
  def expectStructReference() = {
    kind match {
      case at3 @ StructRef3(_, _) => Reference3[StructRef3](ownership, at3)
    }
  }
  def expectFunctionReference() = {
    kind match {
      case at3 @ FunctionT3(_, _) => Reference3[FunctionT3](ownership, at3)
    }
  }
}

sealed trait Referend3

case class Int3() extends Referend3
case class Bool3() extends Referend3
case class Str3() extends Referend3
case class Void3() extends Referend3
case class Float3() extends Referend3
case class Never3() extends Referend3

case class InterfaceRef3(interfaceId: Int, fullName: FullName3) extends Referend3

case class StructRef3(structId: Int, fullName: FullName3) extends Referend3

case class FunctionT3(
    paramTypes: List[Reference3[Referend3]],
    returnType: Reference3[Referend3]
) extends Referend3

case class RawArrayT3(elementType: Reference3[Referend3])
case class UnknownSizeArrayT3(rawArray: RawArrayT3) extends Referend3
case class KnownSizeArrayT3(size: Int, rawArray: RawArrayT3) extends Referend3



// These are a lowered form of the templar's templatas, we need to preserve the (lowered) information
// so we can uniquely identify things.
sealed trait ITemplata3
case class CoordTemplata3(reference: Reference3[Referend3]) extends ITemplata3
case class KindTemplata3(referend: Referend3) extends ITemplata3
case class ArrayTemplateTemplata3() extends ITemplata3
case class FunctionTemplata3(envName: FullName3, humanName: String, location: CodeLocation) extends ITemplata3
case class StructTemplata3(envName: FullName3, humanName: String, location: CodeLocation) extends ITemplata3
case class InterfaceTemplata3(envName: FullName3, humanName: String, location: CodeLocation) extends ITemplata3
case class ImplTemplata3(envName: FullName3, location: CodeLocation) extends ITemplata3
case class ExternFunctionTemplata3(fullName: FullName3) extends ITemplata3
case class OwnershipTemplata3(ownership: Ownership) extends ITemplata3
case class VariabilityTemplata3(variability: Variability) extends ITemplata3
case class MutabilityTemplata3(mutability: Mutability) extends ITemplata3
case class PermissionTemplata3(mutability: Permission) extends ITemplata3
case class LocationTemplata3(mutability: Location) extends ITemplata3
case class BooleanTemplata3(value: Boolean) extends ITemplata3
case class IntegerTemplata3(value: Integer) extends ITemplata3
