package net.verdagon.radonc.templar.citizen

import net.verdagon.radonc.astronomer.{FunctionA, InterfaceA, StructA, StructMemberA}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.{FinalP, ImmutableP, MutabilityP, MutableP}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.OverloadTemplar.ScoutExpectedFunctionSuccess
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer, FunctionTemplarOrdinaryOrTemplatedLayer}
import net.verdagon.radonc.{vassertSome, vfail, vimpl, vwat}

import scala.collection.immutable.List

object StructTemplarCore {
  // Takes a IEnvironment because we might be inside a:
  // struct:T Thing:T {
  //   t: T;
  // }
  // which means we need some way to know what T is.
  def makeInterface(
    interfaceRunesEnv: NamespaceEnvironment,
    temputs0: Temputs,
    interface1: InterfaceA,
    coercedFinalTemplateArgs2: List[ITemplata]):
  (Temputs, InterfaceDefinition2) = {
    if (interface1.namespace.nonEmpty) {
      vimpl()
    }
    val fullName = FullName2(List(NamePart2(interface1.name, Some(coercedFinalTemplateArgs2))))
    val temporaryInferfaceRef = InterfaceRef2(fullName)

    val interfaceInnerEnv =
      NamespaceEnvironment(
        Some(interfaceRunesEnv),
        fullName,
        interface1.identifyingRunes.zip(coercedFinalTemplateArgs2)
          .map({ case (rune, templata) => (rune, List(TemplataEnvEntry(templata))) })
          .toMap)

    val temputs2 =
      temputs0
        .declareInterfaceEnv(
          temporaryInferfaceRef,
          interfaceInnerEnv)

    val interfaceDef2 =
      InterfaceDefinition2(
        fullName,
        Conversions.evaluateMutability(interface1.mutability))
    val temputs14 = temputs2.add(interfaceDef2)

    val (temputs15, _) = ImplTemplar.getParentInterfaces(temputs14, temporaryInferfaceRef)

    val temputs20 = temputs15
//    val temputs20 =
//      interface1.internalMethods.foldLeft(temputs15)({
//        case (temputs18, ntvFunction1) => {
//          if (ntvFunction1.isTemplate) {
//            // Do nothing, can't evaluate it now
//            temputs18
//          } else {
//            FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
//              temputs18,
//              FunctionTemplata(interfaceInnerEnv, ntvFunction1))
//          }
//        }
//      })

    (temputs20, interfaceDef2)
  }

  def makeStruct(
    // The environment that the struct was defined in.
    structRunesEnv: NamespaceEnvironment,
    temputs0: Temputs,
    struct1: StructA,
    coercedFinalTemplateArgs: List[ITemplata]):
  (Temputs, StructDefinition2) = {
    if (struct1.namespace.nonEmpty) {
      vimpl()
    }
    val fullName = FullName2(List(NamePart2(struct1.name, Some(coercedFinalTemplateArgs))))
    val temporaryStructRef = StructRef2(fullName)

    val structInnerEnv =
      NamespaceEnvironment(
        Some(structRunesEnv),
        fullName,
        Map())
    // when we have structs that contain functions, add this back in
//        struct1.members
//          .map(_.origin)
//          .map(FunctionEnvEntry)
//          .groupBy(_.function.name))

    val temputs1 =
      temputs0
        .declareStructEnv(
          temporaryStructRef,
          structInnerEnv)

    val (temputs2, members) = makeStructMembers(structInnerEnv, temputs1, struct1.members)

    val structDef2 =
      StructDefinition2(
        fullName,
        Conversions.evaluateMutability(struct1.mutability),
        members)

    val temputs3 = temputs2.add(structDef2);

//    // Lets make a constructor first.
//    // We technically don't have to do it here; it could be made lazily since
//    // the FunctionEnvEntry is already in the env.
//    val (temputs10, _) =
//      OverloadTemplar.scoutExpectedFunctionForPrototype(
//        structInnerEnv,
//        temputs3,
//        struct1.name,
//        coercedFinalTemplateArgs,
//        members
//          .map({ case StructMember2(_, _, ReferenceMemberType2(coord)) => coord})
//          .map(coord => ParamFilter(coord, None)),
//        true)

    val (temputs14, implementedInterfaceRefs2) =
      ImplTemplar.getParentInterfaces(temputs3, temporaryStructRef);

    val temputs18 =
      implementedInterfaceRefs2.foldLeft(temputs14)({
        case (temputs16, implementedInterfaceRef2) => {
          val ownership = if (structDef2.mutability == Mutable) Own else Share
          val (temputs17, ScoutExpectedFunctionSuccess(_)) =
            OverloadTemplar.scoutExpectedFunctionForPrototype(
              structInnerEnv,
              temputs16,
              CallTemplar.INTERFACE_DESTRUCTOR_NAME,
              List(),
              List(ParamFilter(Coord(ownership, structDef2.getRef), Some(Override2(implementedInterfaceRef2)))),
              true)
          temputs17
        }
      })

    val (temputs21, ancestorInterfaces) =
      ImplTemplar.getAncestorInterfaces(temputs18, temporaryStructRef)
    val temputs30 =
      ancestorInterfaces.foldLeft(temputs21)({
        case (temputs11, ancestorInterface) => {
          temputs11.addImpl(temporaryStructRef, ancestorInterface)
        }
      })

    (temputs30, structDef2)
  }

  private def makeStructMembers(env: IEnvironment, temputs0: Temputs, members: List[StructMemberA]): (Temputs, List[StructMember2]) = {
    members match {
      case Nil => (temputs0, Nil)
      case head1 :: tail1 => {
        val (temputs1, head2) = makeStructMember(env, temputs0, head1);
        val (temputs2, tail2) = makeStructMembers(env, temputs1, tail1);
        (temputs2, head2 :: tail2)
      }
    }
  }

  private def makeStructMember(
    env: IEnvironment,
    temputs0: Temputs,
    member: StructMemberA):
  (Temputs, StructMember2) = {
    val CoordTemplata(coord) = vassertSome(env.getNearestTemplataWithName(member.typeRune, Set(TemplataLookupContext)))
    (temputs0, StructMember2(member.name, Conversions.evaluateVariability(member.variability), ReferenceMemberType2(coord)))
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    env: IEnvironment,
    temputs0: Temputs,
    functionS: FunctionA,
    functionFullName: FullName2,
    members: List[StructMember2]):
  (Temputs, StructRef2, Mutability, FunctionTemplata) = {
    val mutability =
      getCompoundTypeMutability(temputs0, members.map(_.tyype.reference))

    val nearName = FunctionScout.CLOSURE_STRUCT_NAME + functionS.name // For example "__Closure:main:lam1"
    val fullName = FullName2(functionFullName.steps :+ NamePart2(nearName, Some(List())))

    val structRef = StructRef2(fullName)

    // We declare the function into the environment that we use to compile the
    // struct, so that those who use the struct can reach into its environment
    // and see the function and use it.
    // See CSFMSEO and SAFHE.
    val structEnv =
      NamespaceEnvironment(
        Some(env),
        fullName,
        Map(
          CallTemplar.CALL_FUNCTION_NAME -> List(FunctionEnvEntry(functionS)),
          nearName -> List(TemplataEnvEntry(KindTemplata(structRef))),
          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME -> List(TemplataEnvEntry(KindTemplata(structRef)))))
    // We return this from the function in case we want to eagerly compile it (which we do
    // if it's not a template).
    val functionTemplata = FunctionTemplata(structEnv, functionS)

    val temputs1 = temputs0.declareStruct(structRef);
    val temputs2 =
      temputs1
        .declareStructMutability(structRef, mutability)
        .declareStructEnv(structRef, structEnv);

    val closureStructDefinition = StructDefinition2(fullName, mutability, members);
    val temputs3 = temputs2.add(closureStructDefinition)

    val closuredVarsStructRef = closureStructDefinition.getRef;

    (temputs3, closuredVarsStructRef, mutability, functionTemplata)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    memberCoords: List[Coord],
    prefix: String):
  (Temputs, StructRef2, Mutability) = {
    temputs0.packTypes.get(memberCoords) match {
      case Some(structRef2) => (temputs0, structRef2, temputs0.lookupStruct(structRef2).mutability)
      case None => {
        val packMutability = getCompoundTypeMutability(temputs0, memberCoords)
        val members =
          memberCoords.zipWithIndex.map({
            case (pointerType, index) => StructMember2(index.toString, Final, ReferenceMemberType2(pointerType))
          })

        val memberCoordTemplatas = memberCoords.map(CoordTemplata)
        val templateArgs = memberCoordTemplatas

        val fullName = FullName2(List(NamePart2(prefix, Some(templateArgs))))

        val newStructDef = StructDefinition2(fullName, packMutability, members);
        if (memberCoords.isEmpty && packMutability != Immutable)
          vfail("curiosity")

        val temputs1 = temputs0.declareStruct(newStructDef.getRef);
        val temputs2 =
          temputs1
            .declareStructMutability(newStructDef.getRef, packMutability)
            .declareStructEnv(newStructDef.getRef, env);
        val temputs3 = temputs2.add(newStructDef)
        val temputs4 = temputs3.declarePack(memberCoords, newStructDef.getRef);

        (temputs4, newStructDef.getRef, packMutability)
      }
    }
  }

  def getCompoundTypeMutability(temputs0: Temputs, memberTypes2: List[Coord])
  : Mutability = {
    val membersOwnerships = memberTypes2.map(_.ownership)
    val allMembersImmutable = membersOwnerships.toSet == Set(Share)
    if (allMembersImmutable) Immutable else Mutable
  }
}
