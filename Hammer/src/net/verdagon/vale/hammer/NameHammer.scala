package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.scout.CodeLocationS
import net.verdagon.vale.templar.{FullName2, IName2, Templar}
import net.verdagon.vale.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail, vimpl}
import net.verdagon.von.VonStr

import scala.collection.immutable.List

object NameHammer {
  // This is here temporarily until we make INameH fully support stuff.
  // Maybe we want INameH to be more general, so people downstream dont have to support stuff?
  // Maybe make a SpecialNodeH(Str) or something.
  def stringify(name: FullName2[IName2]): String = {
    name.toString
  }
  def stringifyNamePart(name: IName2): String = {
    name.toString
  }

  def translateName(hinputs: Hinputs, hamuts: HamutsBox, fullName2: FullName2[IName2]): FullNameH = {
    FullNameH(fullName2.steps.map(step => VonStr(stringifyNamePart(step))))
  }

//  def translateContainer(container: IContainer): IContainerH = {
//    container match {
//      case ContainerInterface(interface) => IContainerH(interface.name, Conversions.evaluateCodeLocation(interface.codeLocation))
//      case ContainerStruct(struct) => IContainerH(struct.name, Conversions.evaluateCodeLocation(struct.codeLocation))
//      case ContainerFunction(function) => IContainerH(function.name, Conversions.evaluateCodeLocation(function.codeLocation))
//      case ContainerImpl(impl) => IContainerH(Templar.IMPL_NAME, Conversions.evaluateCodeLocation(impl.codeLocation))
//    }
//  }
}
