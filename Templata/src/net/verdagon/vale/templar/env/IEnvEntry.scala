package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.vale.templar.templata.{FunctionHeader2, ITemplata}
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}

// Function/Struct/InterfaceEnvEntry are different from Function/Struct/InterfaceTemplata
// in that the latter have the environment that they came from. Usually we don't have
// the *Templata in the environment directly; when the *EnvEntry is pulled from the
// environment, it's bundled with the environment it came from, into a *Templata.

// The parent is the struct or interface that this env entry is inside.
// E.g. if LinkedList has a Node substruct, then the Node struct will be in a
// StructEnvEntry, which will have a parent LinkedList StructEnvEntry.

sealed trait IEnvEntry { }
case class FunctionEnvEntry(parent: Option[IEnvEntry], function: FunctionA) extends IEnvEntry
case class ImplEnvEntry(parent: Option[IEnvEntry], impl: ImplA) extends IEnvEntry
case class StructEnvEntry(parent: Option[IEnvEntry], struct: StructA) extends IEnvEntry
case class InterfaceEnvEntry(parent: Option[IEnvEntry], interface: InterfaceA) extends IEnvEntry
// No particular reason we don't have ancestors in here, but no reason to have them either so
// they're left out for now.
case class TemplataEnvEntry(templata: ITemplata) extends IEnvEntry
