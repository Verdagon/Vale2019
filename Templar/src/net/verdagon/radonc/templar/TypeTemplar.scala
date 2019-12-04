package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc.astronomer.ITemplexA
import net.verdagon.radonc.templar.citizen.{ImplTemplar, StructTemplar}
import net.verdagon.radonc.templar.env.IEnvironment
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types._

import scala.collection.immutable.List
//import net.verdagon.radonc.carpenter.CovarianceCarpenter
import net.verdagon.radonc.scout._

object TypeTemplar {

  def evaluateAndReferencifyType(env: IEnvironment, temputs0: Temputs, type1: ITemplexA, ownershipIfMutable: Ownership):
  (Temputs, Coord) = {
    val (temputs1, typeTemplata) = TemplataTemplar.evaluateTemplex(env, temputs0, type1)
    typeTemplata match {
      case st @ StructTemplata(_, _) => {
        val (temputs1, structRef) =
          StructTemplar.getStructRef(temputs0, st, List())
        (temputs1, TemplataTemplar.pointifyReferend(temputs1, structRef, ownershipIfMutable))
      }
      case st @ InterfaceTemplata(_, _) => {
        val (temputs1, interfaceRef) =
          StructTemplar.getInterfaceRef(temputs0, st, List())
        (temputs1, TemplataTemplar.pointifyReferend(temputs1, interfaceRef, ownershipIfMutable))
      }
      case CoordTemplata(r) => (temputs1, r)
      case KindTemplata(referend) => {
        (temputs1, TemplataTemplar.pointifyReferend(temputs1, referend, ownershipIfMutable))
      }
    }
  }

  def convertExprs(
      env: IEnvironment,
      temputs0: Temputs,
      sourceExprs: List[ReferenceExpression2],
      targetPointerTypes: List[Coord]):
  (Temputs, List[ReferenceExpression2]) = {
    if (sourceExprs.size != targetPointerTypes.size) {
      vfail("num exprs mismatch, source:\n" + sourceExprs + "\ntarget:\n" + targetPointerTypes)
    }
    (sourceExprs zip targetPointerTypes).foldLeft((temputs0, List[ReferenceExpression2]()))({
      case ((temputs1, previousRefExprs), (sourceExpr, targetPointerType)) => {
        val (temputs2, refExpr) =
          convert(env, temputs1, sourceExpr, targetPointerType)
        (temputs2, previousRefExprs :+ refExpr)
      }
    })
  }

  def convert(
      env: IEnvironment,
      temputs0: Temputs,
      sourceExpr: ReferenceExpression2,
      targetPointerType: Coord):
  (Temputs, ReferenceExpression2) = {
    val sourcePointerType = sourceExpr.resultRegister.reference

    val Coord(targetOwnership, targetType) = targetPointerType;
    val Coord(sourceOwnership, sourceType) = sourcePointerType;

    if (sourceType == Never2()) {
      return (temputs0, TemplarReinterpret2(sourceExpr, targetPointerType))
    }

    val sourceExprDecayedOwnershipped =
      (sourceOwnership, targetOwnership) match {
        case (Own, Own) => sourceExpr
        case (Borrow, Own) => {
          vfail("Supplied a borrow but target wants to own the argument")
        }
        case (Own, Borrow) => {
          vfail("Supplied an owning but target wants to only borrow")
        }
        case (Borrow, Borrow) => sourceExpr
        case (Raw, Raw) => sourceExpr
        case (Share, Share) => sourceExpr
        case (Own, Share) => {
          vfail(); // curious
        }
        case (Borrow, Share) => {
          vfail(); // curious
        }
      }

    val (temputs1, sourceExprDecayedOwnershippedConverted) =
      if (sourceType == targetType) {
        (temputs0, sourceExprDecayedOwnershipped)
      } else {
        (sourceType, targetType) match {
          case (s @ StructRef2(_), i : InterfaceRef2) => {
            StructTemplar.convert(env.globalEnv, temputs0, sourceExprDecayedOwnershipped, s, i)
          }
          case _ => vfail()
        }
      };

    (temputs1, sourceExprDecayedOwnershippedConverted)
  }
}