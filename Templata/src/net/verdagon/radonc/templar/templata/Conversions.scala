package net.verdagon.radonc.templar.templata

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.vimpl

object Conversions {
  def evaluateMutability(mutability: MutabilityP): Mutability = {
    mutability match {
      case MutableP => Mutable
      case ImmutableP => Immutable
    }
  }

  def evaluatePermission(permission: PermissionP): Permission = {
    permission match {
      case ReadonlyP => Readonly
      case ReadwriteP => Readwrite
      case ExclusiveReadwriteP => ExclusiveReadwrite
    }
  }

  def evaluateLocation(location: LocationP): Location = {
    location match {
      case InlineP => Inline
      case YonderP => Yonder
    }
  }

  def evaluateVariability(variability: VariabilityP): Variability = {
    variability match {
      case FinalP => Final
      case VaryingP => Varying
    }
  }

  def evaluateOwnership(ownership: OwnershipP): Ownership = {
    ownership match {
      case OwnP => Own
      case BorrowP => Borrow
      case ShareP => Share
      case RawP => Raw
    }
  }

  def unevaluateOwnership(ownership: Ownership): OwnershipP = {
    ownership match {
      case Own => OwnP
      case Borrow => BorrowP
      case Share => ShareP
      case Raw => RawP
    }
  }

  def unevaluateTemplataType(tyype: ITemplataType): ITypeSR = {
    tyype match {
      case CoordTemplataType => CoordTypeSR
      case KindTemplataType => KindTypeSR
      case IntegerTemplataType => IntTypeSR
      case BooleanTemplataType => BoolTypeSR
      case MutabilityTemplataType => MutabilityTypeSR
      case PermissionTemplataType => PermissionTypeSR
      case LocationTemplataType => LocationTypeSR
      case OwnershipTemplataType => OwnershipTypeSR
      case VariabilityTemplataType => VariabilityTypeSR
      case TemplateTemplataType(_, _) => vimpl() // can we even specify template types in the syntax?
    }
  }
}
