package net.verdagon.vale.astronomer

import net.verdagon.vale.parser._
import net.verdagon.vale.scout._
import net.verdagon.vale.{vassert, vwat}

import scala.collection.immutable.List

trait IExpressionAE

case class ProgramA(
    structs: List[StructA],
    interfaces: List[InterfaceA],
    impls: List[ImplA],
    functions: List[FunctionA]) {
  def lookupFunction(name: String) = {
    val matches = functions.filter(_.name == name)
    vassert(matches.size == 1)
    matches.head
  }
  def lookupInterface(name: String) = {
    val matches = interfaces.find(_.name == name)
    vassert(matches.size == 1)
    matches.head match {
      case i @ InterfaceA(_, _, _, _, _, _, _, _, _, _) => i
    }
  }
  def lookupStruct(name: String) = {
    val matches = structs.find(_.name == name)
    vassert(matches.size == 1)
    matches.head match {
      case i @ StructA(_, _, _, _, _, _, _, _, _, _) => i
    }
  }
}


trait TypeDefinitionA {
  def name: String;
}

case class StructA(
    codeLocation: CodeLocationS,
    namespace: List[String],
    name: String,
    mutability: MutabilityP,
    maybePredictedMutability: Option[MutabilityP],
    tyype: ITemplataType,
    identifyingRunes: List[String],
    typeByRune: Map[String, ITemplataType],
    rules: List[IRulexAR],
    members: List[StructMemberA]
) extends TypeDefinitionA {
  def isTemplate: Boolean = tyype match {
    case KindTemplataType => false
    case TemplateTemplataType(_, _) => true
    case _ => vwat()
  }
}

case class StructMemberA(
    name: String,
    variability: VariabilityP,
    typeRune: String)

case class ImplA(
    codeLocation: CodeLocationS,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    structKindRune: String,
    interfaceKindRune: String)

//case class AliasA(
//  codeLocation: CodeLocation,
//  rules: List[IRulexAR],
//  typeByRune: Map[String, ITemplataType],
//  aliasRune: String,
//  aliaseeRune: String)

case class InterfaceA(
    codeLocation: CodeLocationS,
    namespace: List[String],
    name: String,
    mutability: MutabilityP,
    maybePredictedMutability: Option[MutabilityP],
    tyype: ITemplataType,
    identifyingRunes: List[String],
    typeByRune: Map[String, ITemplataType],
    rules: List[IRulexAR],
    // See IMRFDI
    internalMethods: List[FunctionA]) {
  def isTemplate: Boolean = tyype match {
    case KindTemplataType => false
    case TemplateTemplataType(_, _) => true
    case _ => vwat()
  }
}

object interfaceName {
  // The extraction method (mandatory)
  def unapply(interfaceA: InterfaceA): Option[String] = {
    Some(interfaceA.name)
  }
}

object structName {
  // The extraction method (mandatory)
  def unapply(structA: StructA): Option[String] = {
    Some(structA.name)
  }
}

// remember, by doing a "m", CaptureSP("m", Destructure("Marine", List("hp, "item"))), by having that
// CaptureSP/"m" there, we're changing the nature of that Destructure; "hp" and "item" will be
// borrows rather than owns.

// So, when the scout is assigning everything a name, it's actually forcing us to always have
// borrowing destructures.

// We should change Scout to not assign names... or perhaps, it can assign names for the parameters,
// but secretly, templar will consider arguments to have actual names of __arg_0, __arg_1, and let
// the PatternTemplar introduce the actual names.

// Also remember, if a parameter has no name, it can't be varying.

// template params.

// Underlying class for all XYZFunctionS types
case class FunctionA(
    codeLocation: CodeLocationS,
    name: String,
    namespace: List[String],
    lambdaNumber: Int, // 0 if at top level
    isUserFunction: Boolean,

    tyype: ITemplataType,
    // This is not necessarily only what the user specified, the compiler can add
    // things to the end here, see CCAUIR.
    identifyingRunes: List[String],
    typeByRune: Map[String, ITemplataType],

    params: List[ParameterS],

    // We need to leave it an option to signal that the compiler can infer the return type.
    maybeRetCoordRune: Option[String],

    templateRules: List[IRulexAR],
    body: IBodyA
) {
  def isLight(): Boolean = {
    lambdaNumber == 0 ||
      (body match {
        case ExternBodyA => true
        case AbstractBodyA => true
        case GeneratedBodyA(_) => true
        case CodeBodyA(body1) => body1.closuredNames.isEmpty
      })
  }

  def isTemplate: Boolean = tyype match {
    case FunctionTemplataType => false
    case TemplateTemplataType(_, _) => true
    case _ => vwat()
  }
}

sealed trait IBodyA
case object ExternBodyA extends IBodyA
case object AbstractBodyA extends IBodyA
case class GeneratedBodyA(generatorId: String) extends IBodyA
case class CodeBodyA(bodyA: BodyAE) extends IBodyA

case class BFunctionA(
  origin: FunctionA,
  name: String,
  body: BodyAE)
