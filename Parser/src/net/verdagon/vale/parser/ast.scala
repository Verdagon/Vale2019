// good

package net.verdagon.vale.parser

import scala.util.parsing.input.Positional

case class Program0(
    structs: List[StructP],
    interfaces: List[InterfaceP],
    impls: List[ImplP],
    functions: List[FunctionP])

case class ImplP(
  // Impls don't need ordered runes, see IDHIR.
  rules: List[IRulexPR],
  struct: ITemplexPPT,
  interface: ITemplexPPT) extends Positional

case class StructP(
    name: String,
    mutability: MutabilityP,
    identifyingRunes: Option[List[String]],
    templateRules: List[IRulexPR],
    members: List[StructMemberP]) extends Positional

case class StructMemberP(
  name: String,
  variability: VariabilityP,
  tyype: ITemplexPT)

case class InterfaceP(
    name: String,
    mutability: MutabilityP,
    maybeIdentifyingRunes: Option[List[String]],
    templateRules: List[IRulexPR],
    members: List[FunctionP]) extends Positional

case class FunctionP(
  name: Option[String],
  isExtern: Boolean,
  isAbstract: Boolean,
  isUserFunction: Boolean,

  userSpecifiedIdentifyingRunes: List[String],
  templateRules: List[IRulexPR],

  params: List[PatternPP],
  ret: Option[ITemplexPPT],
  body: Option[BlockPE]) extends Positional




sealed trait MutabilityP
case object MutableP extends MutabilityP
case object ImmutableP extends MutabilityP

sealed trait VariabilityP
case object FinalP extends VariabilityP
case object VaryingP extends VariabilityP

sealed trait OwnershipP
case object OwnP extends OwnershipP
case object BorrowP extends OwnershipP
case object ShareP extends OwnershipP

sealed trait PermissionP
case object ReadonlyP extends PermissionP
case object ReadwriteP extends PermissionP
case object ExclusiveReadwriteP extends PermissionP

sealed trait LocationP
case object InlineP extends LocationP
case object YonderP extends LocationP
