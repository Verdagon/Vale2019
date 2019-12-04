package net.verdagon.radonc.scout

import net.verdagon.radonc.parser._
import net.verdagon.radonc.vcurious

import scala.collection.immutable.List

// See PVSBUFI
sealed trait ITemplexS
case class IntST(value: Int) extends ITemplexS
case class MutabilityST(mutability: MutabilityP) extends ITemplexS
case class PermissionST(permission: PermissionP) extends ITemplexS
case class LocationST(location: LocationP) extends ITemplexS
case class OwnershipST(ownership: OwnershipP) extends ITemplexS
case class VariabilityST(variability: VariabilityP) extends ITemplexS
case class BoolST(value: Boolean) extends ITemplexS
case class NameST(name: String) extends ITemplexS
case class RuneST(rune: String) extends ITemplexS
case class AnonymousRuneST() extends ITemplexS
case class OwnershippedST(ownership: OwnershipP, inner: ITemplexS) extends ITemplexS
case class NullableST(inner: ITemplexS) extends ITemplexS
case class CallST(
    template: ITemplexS,
    args: List[ITemplexS]) extends ITemplexS {
}
//case class FunctionST(
//  mutability: Option[ITemplexS],
//  parameters: List[Option[ITemplexS]],
//  returnType: Option[ITemplexS]
//) extends ITemplexS
case class PrototypeST(
  name: String,
  parameters: List[ITemplexS],
  returnType: ITemplexS
) extends ITemplexS
case class PackST(
  members: List[ITemplexS]
) extends ITemplexS
case class RepeaterSequenceST(
  mutability: ITemplexS,
  size: ITemplexS,
  element: ITemplexS
) extends ITemplexS
case class ManualSequenceST(
  elements: List[ITemplexS]
) extends ITemplexS

object TemplexSUtils {
  def getDistinctOrderedRunesForTemplex(templex: ITemplexS): List[String] = {
    templex match {
      case IntST(value) => List()
      case MutabilityST(mutability) => List()
      case PermissionST(permission) => List()
      case LocationST(location) => List()
      case OwnershipST(ownership) => List()
      case VariabilityST(variability) => List()
      case BoolST(value) => List()
      case NameST(name) => List()
      case RuneST(rune) => List(rune)
      case AnonymousRuneST() => List()
      case OwnershippedST(_, inner) => getDistinctOrderedRunesForTemplex(inner)
      case CallST(template, args) => {
        (template :: args).flatMap(getDistinctOrderedRunesForTemplex).distinct
      }
      case PrototypeST(name, parameters, returnType) => {
        (parameters :+ returnType).flatMap(getDistinctOrderedRunesForTemplex).distinct
      }
      case PackST(members) => {
        members.flatMap(getDistinctOrderedRunesForTemplex).distinct
      }
      case RepeaterSequenceST(mutability, size, element) => {
        List(mutability, size, element).flatMap(getDistinctOrderedRunesForTemplex).distinct
      }
      case ManualSequenceST(elements) => {
        elements.flatMap(getDistinctOrderedRunesForTemplex).distinct
      }
    }
  }

  // DO NOT COPY this without considering using a traverse pattern like
  // we do elsewhere.
  def templexNamesToRunes(runes: Set[String])(templex: ITemplexS): ITemplexS = {
    templex match {
      case IntST(value) => IntST(value)
      case MutabilityST(mutability) => MutabilityST(mutability)
      case PermissionST(permission) => PermissionST(permission)
      case LocationST(location) => LocationST(location)
      case OwnershipST(ownership) => OwnershipST(ownership)
      case VariabilityST(variability) => VariabilityST(variability)
      case BoolST(value) => BoolST(value)
      case NameST(name) => if (runes.contains(name)) RuneST(name) else NameST(name)
      case RuneST(rune) => RuneST(rune)
      case AnonymousRuneST() => AnonymousRuneST()
      case OwnershippedST(ownership, inner) => OwnershippedST(ownership, templexNamesToRunes(runes)(inner))
      case CallST(template, args) => {
        CallST(
          templexNamesToRunes(runes)(template),
          args.map(templexNamesToRunes(runes)))
      }
      case PrototypeST(name, parameters, returnType) => {
        PrototypeST(
          name,
          parameters.map(templexNamesToRunes(runes)),
          templexNamesToRunes(runes)(returnType))
      }
      case PackST(members) => {
        PackST(members.map(templexNamesToRunes(runes)))
      }
      case RepeaterSequenceST(mutability, size, element) => {
        RepeaterSequenceST(
          templexNamesToRunes(runes)(mutability),
          templexNamesToRunes(runes)(size),
          templexNamesToRunes(runes)(element))
      }
      case ManualSequenceST(elements) => {
        ManualSequenceST(elements.map(templexNamesToRunes(runes)))
      }
    }
  }
}