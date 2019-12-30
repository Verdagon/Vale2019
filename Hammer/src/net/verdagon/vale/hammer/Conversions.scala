package net.verdagon.vale.hammer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.metal._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar.{types => t}
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.{vimpl, scout => s}

object Conversions {
  def evaluateCodeLocation(loc: s.CodeLocationS): m.CodeLocation = {
    val s.CodeLocationS(file, line, col) = loc
    m.CodeLocation(file, line, col)
  }

  def evaluateMutability(mutability: t.Mutability): Mutability = {
    mutability match {
      case t.Mutable => Mutable
      case t.Immutable => Immutable
    }
  }

  def evaluatePermission(permission: t.Permission): Permission = {
    permission match {
      case t.Readonly => Readonly
      case t.Readwrite => Readwrite
      case t.ExclusiveReadwrite => ExclusiveReadwrite
    }
  }

  def evaluateLocation(location: t.Location): Location = {
    location match {
      case t.Inline => Inline
      case t.Yonder => Yonder
    }
  }

  def evaluateVariability(variability: t.Variability): Variability = {
    variability match {
      case t.Final => Final
      case t.Varying => Varying
    }
  }

  def evaluateOwnership(ownership: t.Ownership): Ownership = {
    ownership match {
      case t.Own => Own
      case t.Borrow => Borrow
      case t.Share => Share
      case t.Raw => Raw
    }
  }

  def evaluateRefCountCategory(refCountCategory: t.RefCountCategory): m.RefCountCategory = {
    refCountCategory match {
      case t.MemberRefCount => m.MemberRefCount
      case t.VariableRefCount => m.VariableRefCount
      case t.RegisterRefCount => m.RegisterRefCount
    }
  }

  def unevaluateOwnership(ownership: Ownership): m.Ownership = {
    ownership match {
      case Own => m.Own
      case Borrow => m.Borrow
      case Share => m.Share
      case Raw => m.Raw
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
