package net.verdagon.vale.hammer

import net.verdagon.vale.scout.CodeLocation
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vfail

case class ReferenceH[+T <: ReferendH](ownership: Ownership, kind: T) {
  kind match {
    case IntH() | BoolH() | StrH() | FloatH() => {
      if (ownership != Share) {
        vfail("wot")
      }
    }
    case VoidH() => {
      if (ownership != Raw) {
        vfail("wot")
      }
    }
    case _ =>
  }

  def expectKnownSizeArrayReference() = {
    kind match {
      case atH @ KnownSizeArrayTH(_, _) => ReferenceH[KnownSizeArrayTH](ownership, atH)
    }
  }
  def expectUnknownSizeArrayReference() = {
    kind match {
      case atH @ UnknownSizeArrayTH(_) => ReferenceH[UnknownSizeArrayTH](ownership, atH)
    }
  }
  def expectStructReference() = {
    kind match {
      case atH @ StructRefH(_, _) => ReferenceH[StructRefH](ownership, atH)
    }
  }
}

sealed trait ReferendH

case class IntH() extends ReferendH
case class BoolH() extends ReferendH
case class StrH() extends ReferendH
case class VoidH() extends ReferendH
case class FloatH() extends ReferendH
case class NeverH() extends ReferendH

case class InterfaceRefH(interfaceId: Int, fullName: FullNameH) extends ReferendH

case class StructRefH(structId: Int, fullName: FullNameH) extends ReferendH

case class RawArrayTH(elementType: ReferenceH[ReferendH])
case class UnknownSizeArrayTH(rawArray: RawArrayTH) extends ReferendH
case class KnownSizeArrayTH(size: Int, rawArray: RawArrayTH) extends ReferendH



// These are a lowered form of the templar's templatas, we need to preserve the (lowered) information
// so we can uniquely identify things.
sealed trait ITemplataH
case class CoordTemplataH(reference: ReferenceH[ReferendH]) extends ITemplataH
case class KindTemplataH(referend: ReferendH) extends ITemplataH
case class ArrayTemplateTemplataH() extends ITemplataH
case class FunctionTemplataH(envName: FullNameH, humanName: String, location: CodeLocation) extends ITemplataH
case class StructTemplataH(envName: FullNameH, humanName: String, location: CodeLocation) extends ITemplataH
case class InterfaceTemplataH(envName: FullNameH, humanName: String, location: CodeLocation) extends ITemplataH
case class ImplTemplataH(envName: FullNameH, location: CodeLocation) extends ITemplataH
case class ExternFunctionTemplataH(fullName: FullNameH) extends ITemplataH
case class OwnershipTemplataH(ownership: Ownership) extends ITemplataH
case class VariabilityTemplataH(variability: Variability) extends ITemplataH
case class MutabilityTemplataH(mutability: Mutability) extends ITemplataH
case class PermissionTemplataH(mutability: Permission) extends ITemplataH
case class LocationTemplataH(mutability: Location) extends ITemplataH
case class BooleanTemplataH(value: Boolean) extends ITemplataH
case class IntegerTemplataH(value: Integer) extends ITemplataH
