package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.vale.templar.templata.{FunctionHeader2, ITemplata}
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}

sealed trait IEnvEntry
case class FunctionEnvEntry(function: FunctionA) extends IEnvEntry
case class ImplEnvEntry(impl: ImplA) extends IEnvEntry
case class StructEnvEntry(struct: StructA) extends IEnvEntry
case class InterfaceEnvEntry(interface: InterfaceA) extends IEnvEntry
case class TemplataEnvEntry(templata: ITemplata) extends IEnvEntry
