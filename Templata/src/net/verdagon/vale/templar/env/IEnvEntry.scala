package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.vale.templar.templata.{FunctionHeader2, ITemplata}
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}

// Function/Struct/InterfaceEnvEntry are different from Function/Struct/InterfaceTemplata
// in that the latter have the environment that they came from. Usually we don't have
// the *Templata in the environment directly; when the *EnvEntry is pulled from the
// environment, it's bundled with the environment it came from, into a *Templata.
sealed trait IEnvEntry { }
case class FunctionEnvEntry(function: FunctionA) extends IEnvEntry
case class ImplEnvEntry(impl: ImplA) extends IEnvEntry
case class StructEnvEntry(struct: StructA) extends IEnvEntry
case class InterfaceEnvEntry(interface: InterfaceA) extends IEnvEntry
case class TemplataEnvEntry(templata: ITemplata) extends IEnvEntry
