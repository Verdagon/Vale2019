package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils, VirtualitySP}
import net.verdagon.vale.scout.rules.{IRulexSR, ITypeSR, RuleSUtils, TypedSR}
import net.verdagon.vale.vassert

import scala.collection.immutable.List


//sealed trait MutabilityP
//case object MutableS extends MutabilityP
//case object ImmutableS extends MutabilityP
//
//sealed trait VariabilityP
//case object FinalP extends VariabilityP
//case object VaryingP extends VariabilityP
//
//sealed trait OwnershipS
//case object OwnS extends OwnershipS
//case object BorrowS extends OwnershipS
//case object ShareS extends OwnershipS
//case object RawS extends OwnershipS
//
//sealed trait PermissionS
//case object ReadonlyS extends PermissionS
//case object ReadwriteS extends PermissionS
//case object ExclusiveReadwriteS extends PermissionS
//
//sealed trait LocationS
//case object InlineS extends LocationS
//case object YonderS extends LocationS


trait IExpressionSE

case class ProgramS(
    structs: List[StructS],
    interfaces: List[InterfaceS],
    impls: List[ImplS],
    implementedFunctions: List[FunctionS]) {
  def lookupFunction(name: String) = {
    val matches = implementedFunctions.find(_.name == name)
    vassert(matches.size == 1)
    matches.head
  }
  def lookupInterface(name: String) = {
    val matches = interfaces.find(_.name == name)
    vassert(matches.size == 1)
    matches.head
  }
  def lookupStruct(name: String) = {
    val matches = structs.find(_.name == name)
    vassert(matches.size == 1)
    matches.head
  }
}

case class CodeLocation(
  file: String,
  line: Int,
  char: Int)

case class StructS(
    codeLocation: CodeLocation,
    namespace: List[String],
    name: String,
    mutability: MutabilityP,
    maybePredictedMutability: Option[MutabilityP],
    identifyingRunes: List[String],
    allRunes: Set[String],
    maybePredictedType: Option[ITypeSR],
    isTemplate: Boolean,
    rules: List[IRulexSR],
    members: List[StructMemberS])

case class StructMemberS(
    name: String,
    variability: VariabilityP,
    typeRune: String)

case class ImplS(
    codeLocation: CodeLocation,
    rules: List[IRulexSR],
    allRunes: Set[String],
    isTemplate: Boolean,
    structKindRune: String,
    interfaceKindRune: String)

case class InterfaceS(
    codeLocation: CodeLocation,
    namespace: List[String],
    name: String,
    mutability: MutabilityP,
    maybePredictedMutability: Option[MutabilityP],
    identifyingRunes: List[String],
    allRunes: Set[String],
    maybePredictedType: Option[ITypeSR],
    isTemplate: Boolean,
    rules: List[IRulexSR])

object interfaceSName {
  // The extraction method (mandatory)
  def unapply(interfaceS: InterfaceS): Option[String] = {
    Some(interfaceS.name)
  }
}

object structSName {
  // The extraction method (mandatory)
  def unapply(structS: StructS): Option[String] = {
    Some(structS.name)
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

case class ParameterS(
    // Note the lack of a VariabilityP here. The only way to get a variability is with a Capture.
    pattern: AtomSP) {
  vassert(pattern.name.nonEmpty)

  // The name they supplied, or a generated one. This is actually not used at all by the templar,
  // it's probably only used by IDEs. The templar gets arguments by index.
  def name = pattern.name.get
}

case class SimpleParameter1(
    origin: Option[AtomSP],
    name: String,
    virtuality: Option[VirtualitySP],
    tyype: ITemplexS)

sealed trait IBody1
case object ExternBody1 extends IBody1
case object AbstractBody1 extends IBody1
case class GeneratedBody1(generatorId: String) extends IBody1
case class CodeBody1(body1: BodySE) extends IBody1

// template params.

// Underlying class for all XYZFunctionS types
case class FunctionS(
    codeLocation: CodeLocation,
    name: String,
    namespace: List[String],
    lambdaNumber: Int, // 0 if at top level
    isUserFunction: Boolean,

    // This is not necessarily only what the user specified, the compiler can add
    // things to the end here, see CCAUIR.
    identifyingRunes: List[String],
    allRunes: Set[String],
    maybePredictedType: Option[ITypeSR],

    params: List[ParameterS],

    // We need to leave it an option to signal that the compiler can infer the return type.
    maybeRetCoordRune: Option[String],

    isTemplate: Boolean,
    templateRules: List[IRulexSR],
    body: IBody1
) {
  def isLight(): Boolean = {
    lambdaNumber == 0 ||
      (body match {
        case ExternBody1 => true
        case AbstractBody1 => true
        case GeneratedBody1(_) => true
        case CodeBody1(body1) => body1.closuredNames.isEmpty
      })
  }

  //  def orderedIdentifyingRunes: List[String] = {
//    maybeUserSpecifiedIdentifyingRunes match {
//      case Some(userSpecifiedIdentifyingRunes) => userSpecifiedIdentifyingRunes
//      case None => {
//        // Grab the ones from the patterns.
//        // We don't use the ones from the return type because we won't identify a function
//        // from its return type, see CIFFRT.
//        params.map(_.pattern).flatMap(PatternSUtils.getDistinctOrderedRunesForPattern)
//      }
//    }
//  }

//  // This should start with the original runes from the FunctionP in the same order,
//  // See SSRR.
//  private def orderedRunes: List[String] = {
//    (
//      maybeUserSpecifiedIdentifyingRunes.getOrElse(List()) ++
//      params.map(_.pattern).flatMap(PatternSUtils.getDistinctOrderedRunesForPattern) ++
//      RuleSUtils.getDistinctOrderedRunesForRulexes(templateRules) ++
//      maybeRetCoordRune.toList
//    ).distinct
//  }
}

case class BFunctionS(
  origin: FunctionS,
  name: String,
  body: BodySE)


sealed trait RefCountCategory
case object VariableRefCount extends RefCountCategory
case object MemberRefCount extends RefCountCategory
case object RegisterRefCount extends RefCountCategory
