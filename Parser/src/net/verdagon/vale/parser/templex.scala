package net.verdagon.vale.parser

sealed trait ITemplexPT

case class NullablePT(inner: ITemplexPT) extends ITemplexPT

case class BorrowPT(inner: ITemplexPT) extends ITemplexPT
case class OwnPT(inner: ITemplexPT) extends ITemplexPT
case class AnonymousRunePT() extends ITemplexPT
case class NameOrRunePT(rune: String) extends ITemplexPT
case class SharePT(inner: ITemplexPT) extends ITemplexPT

case class CallPT(template: ITemplexPT, args: List[ITemplexPT]) extends ITemplexPT
case class ArraySequencePT(mutability: ITemplexPT, size: ITemplexPT, element: ITemplexPT) extends ITemplexPT


case class BoolPT(value: Boolean) extends ITemplexPT
case class OwnershipPT(ownership: OwnershipP) extends ITemplexPT
case class MutabilityPT(mutability: MutabilityP) extends ITemplexPT
case class LocationPT(location: LocationP) extends ITemplexPT
case class PermissionPT(permission: PermissionP) extends ITemplexPT