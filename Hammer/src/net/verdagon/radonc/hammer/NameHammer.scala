package net.verdagon.radonc.hammer

import net.verdagon.radonc.hinputs.Hinputs
import net.verdagon.radonc.scout.CodeLocation
import net.verdagon.radonc.templar.env.{IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object NameHammer {
  def translateName(hinputs: Hinputs, hamuts0: Hamuts, fullName2: FullName2): (Hamuts, FullName3) = {
    val (hamuts20, steps3) =
      fullName2.steps.foldLeft((hamuts0, List[NamePart3]()))({
        case ((hamuts3, previousNameParts3), namePart2) => {
          val (hamuts5, namePart3) = translateNamePart(hinputs, hamuts3, namePart2)
          (hamuts5, previousNameParts3 :+ namePart3)
        }
      })
    (hamuts20, FullName3(steps3))
  }

  def translateNamePart(hinputs: Hinputs, hamuts0: Hamuts, namePart2: NamePart2): (Hamuts, NamePart3) = {
    namePart2 match {
      case NamePart2(humanName, None) => (hamuts0, NamePart3(humanName, None))
      case NamePart2(humanName, Some(templateArgs)) => {
        val (hamuts15, templatas3) =
          templateArgs.foldLeft((hamuts0, List[ITemplata3]()))({
            case ((hamuts11, previousTemplatas3), templata2) => {
              val (hamuts12, templata3) = translateTemplata(hinputs, hamuts11, templata2)
              (hamuts12, previousTemplatas3 :+ templata3)
            }
          })
        (hamuts15, NamePart3(humanName, Some(templatas3)))
      }
    }
  }

  def translateTemplata(hinputs: Hinputs, hamuts0: Hamuts, templata: ITemplata): (Hamuts, ITemplata3) = {
    templata match {
      case CoordTemplata(reference) => {
        val (hamuts1, coord3) = TypeHammer.translateReference(hinputs, hamuts0, reference)
        (hamuts1, CoordTemplata3(coord3))
      }
      case KindTemplata(kind) => {
        val (hamuts1, kind3) = TypeHammer.translateKind(hinputs, hamuts0, kind)
        (hamuts1, KindTemplata3(kind3))
      }
      case ArrayTemplateTemplata() => (hamuts0, ArrayTemplateTemplata3())
      case FunctionTemplata(outerEnv, function) => {
        val (hamuts1, outerEnvName3) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, FunctionTemplata3(outerEnvName3, function.name, function.codeLocation))
      }
      case StructTemplata(outerEnv, struct) => {
        val (hamuts1, outerEnvName3) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, StructTemplata3(outerEnvName3, struct.name, struct.codeLocation))
      }
      case InterfaceTemplata(outerEnv, struct) => {
        val (hamuts1, outerEnvName3) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, InterfaceTemplata3(outerEnvName3, struct.name, struct.codeLocation))
      }
      case ImplTemplata(outerEnv, impl) => {
        val (hamuts1, outerEnvName3) = translateName(hinputs, hamuts0, outerEnv.fullName)
        (hamuts1, ImplTemplata3(outerEnvName3, impl.codeLocation))
      }
      case ExternFunctionTemplata(header) => {
        val (hamuts1, outerEnvName3) = translateName(hinputs, hamuts0, header.fullName)
        (hamuts1, ExternFunctionTemplata3(outerEnvName3))
      }
      case OwnershipTemplata(ownership) => (hamuts0, OwnershipTemplata3(ownership))
      case VariabilityTemplata(variability) => (hamuts0, VariabilityTemplata3(variability))
      case MutabilityTemplata(mutability) => (hamuts0, MutabilityTemplata3(mutability))
      case PermissionTemplata(mutability) => (hamuts0, PermissionTemplata3(mutability))
      case LocationTemplata(mutability) => (hamuts0, LocationTemplata3(mutability))
      case BooleanTemplata(value) => (hamuts0, BooleanTemplata3(value))
      case IntegerTemplata(value) => (hamuts0, IntegerTemplata3(value))
    }
  }
}
