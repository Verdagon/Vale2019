package net.verdagon.vale.templar

import net.verdagon.vale.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.vale.templar.types.{FullName2, NamePart2}

object simpleName {
  def apply(name: String): FullName2 = {
    FullName2(List(NamePart2(name, Some(List()))))
  }
  def unapply(fullName: FullName2): Option[String] = {
    fullName.steps.lastOption.map(_.humanName)
  }
}

object functionName {
  def unapply(function2: Function2): Option[String] = {
    unapply(function2.header)
  }
  def unapply(header: FunctionHeader2): Option[String] = {
    simpleName.unapply(header.fullName)
  }
  def unapply(prototype: Prototype2): Option[String] = {
    simpleName.unapply(prototype.fullName)
  }
}
