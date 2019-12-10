package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc.astronomer.ITemplexA
import net.verdagon.radonc.templar.citizen.{ImplTemplar, StructTemplar}
import net.verdagon.radonc.templar.env.{IEnvironment, IEnvironmentBox}
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types._

import scala.collection.immutable.List
//import net.verdagon.radonc.carpenter.CovarianceCarpenter
import net.verdagon.radonc.scout._

object TypeTemplar {

  def evaluateAndReferencifyType(env: IEnvironment, temputs: TemputsBox, type1: ITemplexA, ownershipIfMutable: Ownership):
  (Coord) = {
    val typeTemplata = TemplataTemplar.evaluateTemplex(env, temputs, type1)
    typeTemplata match {
      case st @ StructTemplata(_, _) => {
        val structRef =
          StructTemplar.getStructRef(temputs, st, List())
        (TemplataTemplar.pointifyReferend(temputs, structRef, ownershipIfMutable))
      }
      case st @ InterfaceTemplata(_, _) => {
        val interfaceRef =
          StructTemplar.getInterfaceRef(temputs, st, List())
        (TemplataTemplar.pointifyReferend(temputs, interfaceRef, ownershipIfMutable))
      }
      case CoordTemplata(r) => (r)
      case KindTemplata(referend) => {
        (TemplataTemplar.pointifyReferend(temputs, referend, ownershipIfMutable))
      }
    }
  }

  def convertExprs(
      env: IEnvironment,
      temputs: TemputsBox,
      sourceExprs: List[ReferenceExpression2],
      targetPointerTypes: List[Coord]):
  (List[ReferenceExpression2]) = {
    if (sourceExprs.size != targetPointerTypes.size) {
      vfail("num exprs mismatch, source:\n" + sourceExprs + "\ntarget:\n" + targetPointerTypes)
    }
    (sourceExprs zip targetPointerTypes).foldLeft((List[ReferenceExpression2]()))({
      case ((previousRefExprs), (sourceExpr, targetPointerType)) => {
        val refExpr =
          convert(env, temputs, sourceExpr, targetPointerType)
        (previousRefExprs :+ refExpr)
      }
    })
  }

  def convert(
      env: IEnvironment,
      temputs: TemputsBox,
      sourceExpr: ReferenceExpression2,
      targetPointerType: Coord):
  (ReferenceExpression2) = {
    val sourcePointerType = sourceExpr.resultRegister.reference

    val Coord(targetOwnership, targetType) = targetPointerType;
    val Coord(sourceOwnership, sourceType) = sourcePointerType;

    if (sourceType == Never2()) {
      return (TemplarReinterpret2(sourceExpr, targetPointerType))
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

    val sourceExprDecayedOwnershippedConverted =
      if (sourceType == targetType) {
        (sourceExprDecayedOwnershipped)
      } else {
        (sourceType, targetType) match {
          case (s @ StructRef2(_), i : InterfaceRef2) => {
            StructTemplar.convert(env.globalEnv, temputs, sourceExprDecayedOwnershipped, s, i)
          }
          case _ => vfail()
        }
      };

    (sourceExprDecayedOwnershippedConverted)
  }
}