package net.verdagon.radonc.astronomer

import scala.collection.immutable.List

sealed trait ITemplataType
case object CoordTemplataType extends ITemplataType
case object KindTemplataType extends ITemplataType
case object FunctionTemplataType extends ITemplataType
case object IntegerTemplataType extends ITemplataType
case object BooleanTemplataType extends ITemplataType
case object MutabilityTemplataType extends ITemplataType
case object PermissionTemplataType extends ITemplataType
case object LocationTemplataType extends ITemplataType
case object OwnershipTemplataType extends ITemplataType
case object VariabilityTemplataType extends ITemplataType
// This is CitizenTemplataType instead of separate ones for struct and interface
// because the RuleTyper doesn't care whether something's a struct or an interface.
case class TemplateTemplataType(
  paramTypes: List[ITemplataType],
  returnType: ITemplataType
) extends ITemplataType
