package net.verdagon.radonc.templar.env

import net.verdagon.radonc.astronomer.{FunctionA, ImplA, InterfaceA, StructA}
import net.verdagon.radonc.templar.templata.ITemplata

sealed trait IEnvEntry { }
case class FunctionEnvEntry(function: FunctionA) extends IEnvEntry
case class ImplEnvEntry(impl: ImplA) extends IEnvEntry
case class StructEnvEntry(struct: StructA) extends IEnvEntry
case class InterfaceEnvEntry(interface: InterfaceA) extends IEnvEntry
case class TemplataEnvEntry(templata: ITemplata) extends IEnvEntry