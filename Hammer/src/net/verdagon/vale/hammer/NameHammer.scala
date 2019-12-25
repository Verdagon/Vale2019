package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.scout.CodeLocation
import net.verdagon.vale.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail}

import scala.collection.immutable.List

object NameHammer {
  def translateName(hinputs: Hinputs, hamuts0: Hamuts, fullName2: FullName2): (Hamuts, FullNameH) = {
    val (hamuts20, stepsH) =
      fullName2.steps.foldLeft((hamuts0, List[NamePartH]()))({
        case ((hamuts3, previousNamePartsH), namePart2) => {
          val (hamuts5, namePartH) = translateNamePart(hinputs, hamuts3, namePart2)
          (hamuts5, previousNamePartsH :+ namePartH)
        }
      })
    (hamuts20, FullNameH(stepsH))
  }

  def translateNamePart(hinputs: Hinputs, hamuts0: Hamuts, namePart2: NamePart2): (Hamuts, NamePartH) = {
    namePart2 match {
      case NamePart2(humanName, None) => (hamuts0, NamePartH(humanName, None))
      case NamePart2(humanName, Some(templateArgs)) => {
        val (hamuts15, templatasH) =
          templateArgs.foldLeft((hamuts0, List[ITemplataH]()))({
            case ((hamuts11, previousTemplatasH), templata2) => {
              val (hamuts12, templataH) = translateTemplata(hinputs, hamuts11, templata2)
              (hamuts12, previousTemplatasH :+ templataH)
            }
          })
        (hamuts15, NamePartH(humanName, Some(templatasH)))
      }
    }
  }

  def translateTemplata(hinputs: Hinputs, hamuts0: Hamuts, templata: ITemplata): (Hamuts, ITemplataH) = {
    templata match {
      case CoordTemplata(reference) => {
        val (hamuts1, coordH) = TypeHammer.translateReference(hinputs, hamuts0, reference)
        (hamuts1, CoordTemplataH(coordH))
      }
      case KindTemplata(kind) => {
        val (hamuts1, kindH) = TypeHammer.translateKind(hinputs, hamuts0, kind)
        (hamuts1, KindTemplataH(kindH))
      }
      case ArrayTemplateTemplata() => (hamuts0, ArrayTemplateTemplataH())
      case FunctionTemplata(outerEnv, function) => {
        val (hamuts1, outerEnvNameH) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, FunctionTemplataH(outerEnvNameH, function.name, Conversions.evaluateCodeLocation(function.codeLocation)))
      }
      case StructTemplata(outerEnv, struct) => {
        val (hamuts1, outerEnvNameH) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, StructTemplataH(outerEnvNameH, struct.name, Conversions.evaluateCodeLocation(struct.codeLocation)))
      }
      case InterfaceTemplata(outerEnv, struct) => {
        val (hamuts1, outerEnvNameH) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, InterfaceTemplataH(outerEnvNameH, struct.name, Conversions.evaluateCodeLocation(struct.codeLocation)))
      }
      case ImplTemplata(outerEnv, impl) => {
        val (hamuts1, outerEnvNameH) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, ImplTemplataH(outerEnvNameH, Conversions.evaluateCodeLocation(impl.codeLocation)))
      }
      case ExternFunctionTemplata(header) => {
        val (hamuts1, outerEnvNameH) = translateName(hinputs, hamuts0, header.fullName)
        (hamuts1, ExternFunctionTemplataH(outerEnvNameH))
      }
      case OwnershipTemplata(ownership) => (hamuts0, OwnershipTemplataH(Conversions.evaluateOwnership(ownership)))
      case VariabilityTemplata(variability) => (hamuts0, VariabilityTemplataH(Conversions.evaluateVariability(variability)))
      case MutabilityTemplata(mutability) => (hamuts0, MutabilityTemplataH(Conversions.evaluateMutability(mutability)))
      case PermissionTemplata(permission) => (hamuts0, PermissionTemplataH(Conversions.evaluatePermission(permission)))
      case LocationTemplata(location) => (hamuts0, LocationTemplataH(Conversions.evaluateLocation(location)))
      case BooleanTemplata(value) => (hamuts0, BooleanTemplataH(value))
      case IntegerTemplata(value) => (hamuts0, IntegerTemplataH(value))
    }
  }
}
