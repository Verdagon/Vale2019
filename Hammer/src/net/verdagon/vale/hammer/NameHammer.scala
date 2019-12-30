package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.scout.CodeLocationS
import net.verdagon.vale.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail, vimpl}

import scala.collection.immutable.List

object NameHammer {
  def translateName(hinputs: Hinputs, hamuts: HamutsBox, fullName2: FullName2): FullNameH = {
    FullNameH(fullName2.steps.map(translateNamePart(hinputs, hamuts, _)))
  }

  def translateNamePart(hinputs: Hinputs, hamuts: HamutsBox, namePart2: NamePart2): NamePartH = {
    val NamePart2(humanName, maybeTemplateArgs, maybeParameters, maybeCodeLocation) = namePart2
    NamePartH(
      humanName,
      maybeTemplateArgs.map(_.map(translateTemplata(hinputs, hamuts, _))),
      maybeParameters.map(_.map(TypeHammer.translateReference(hinputs, hamuts, _))),
      maybeCodeLocation.map(translateCodeLocation))
  }

  def translateCodeLocation(loc: CodeLocation2): CodeLocationH = {
    val CodeLocation2(file, line, col) = loc
    CodeLocationH(file, line, col)
  }

  def translateTemplata(hinputs: Hinputs, hamuts: HamutsBox, templata: ITemplata): ITemplataH = {
    templata match {
      case CoordTemplata(reference) => {
        val (coordH) = TypeHammer.translateReference(hinputs, hamuts, reference)
        (CoordTemplataH(coordH))
      }
      case KindTemplata(kind) => {
        val (kindH) = TypeHammer.translateKind(hinputs, hamuts, kind)
        (KindTemplataH(kindH))
      }
      case ArrayTemplateTemplata() => ArrayTemplateTemplataH()
      case FunctionTemplata(outerEnv, function) => {
        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
        (FunctionTemplataH(outerEnvNameH, function.name, Conversions.evaluateCodeLocation(function.codeLocation)))
      }
      case StructTemplata(outerEnv, struct) => {
        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
        (StructTemplataH(outerEnvNameH, struct.name, Conversions.evaluateCodeLocation(struct.codeLocation)))
      }
      case InterfaceTemplata(outerEnv, struct) => {
        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
        (InterfaceTemplataH(outerEnvNameH, struct.name, Conversions.evaluateCodeLocation(struct.codeLocation)))
      }
      case ImplTemplata(outerEnv, impl) => {
        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
        (ImplTemplataH(outerEnvNameH, Conversions.evaluateCodeLocation(impl.codeLocation)))
      }
      case ExternFunctionTemplata(header) => {
        val (outerEnvNameH) = translateName(hinputs, hamuts, header.fullName)
        (ExternFunctionTemplataH(outerEnvNameH))
      }
      case OwnershipTemplata(ownership) => OwnershipTemplataH(Conversions.evaluateOwnership(ownership))
      case VariabilityTemplata(variability) => VariabilityTemplataH(Conversions.evaluateVariability(variability))
      case MutabilityTemplata(mutability) => MutabilityTemplataH(Conversions.evaluateMutability(mutability))
      case PermissionTemplata(permission) => PermissionTemplataH(Conversions.evaluatePermission(permission))
      case LocationTemplata(location) => LocationTemplataH(Conversions.evaluateLocation(location))
      case BooleanTemplata(value) => BooleanTemplataH(value)
      case IntegerTemplata(value) => IntegerTemplataH(value)
    }
  }
}
