package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer.{FunctionA, InterfaceA, StructA, StructMemberA}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser.{FinalP, ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.OverloadTemplar.ScoutExpectedFunctionSuccess
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer, FunctionTemplarOrdinaryOrTemplatedLayer}
import net.verdagon.vale.{vassertSome, vfail, vimpl, vwat}

import scala.collection.immutable.List

object StructTemplarCore {
  // Takes a IEnvironment because we might be inside a:
  // struct<T> Thing<T> {
  //   t: T;
  // }
  // which means we need some way to know what T is.
  def makeInterface(
    interfaceRunesEnv: NamespaceEnvironment,
    temputs: TemputsBox,
    interface1: InterfaceA,
    coercedFinalTemplateArgs2: List[ITemplata]):
  (InterfaceDefinition2) = {
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

      temputs
        .declareInterfaceEnv(
          temporaryInferfaceRef,
          interfaceInnerEnv)

    val interfaceDef2 =
      InterfaceDefinition2(
        fullName,
        Conversions.evaluateMutability(interface1.mutability))
    temputs.add(interfaceDef2)

    val _ = ImplTemplar.getParentInterfaces(temputs, temporaryInferfaceRef)

//
//      interface1.internalMethods.foldLeft(temputs)({
//        case (ntvFunction1) => {
//          if (ntvFunction1.isTemplate) {
//            // Do nothing, can't evaluate it now
//            temputs
//          } else {
//            FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
//              temputs,
//              FunctionTemplata(interfaceInnerEnv, ntvFunction1))
//          }
//        }
//      })

    (interfaceDef2)
  }

  def makeStruct(
    // The environment that the struct was defined in.
    structRunesEnv: NamespaceEnvironment,
    temputs: TemputsBox,
    struct1: StructA,
    coercedFinalTemplateArgs: List[ITemplata]):
  (StructDefinition2) = {
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


      temputs
        .declareStructEnv(
          temporaryStructRef,
          structInnerEnv)

    val members = makeStructMembers(structInnerEnv, temputs, struct1.members)

    val structDef2 =
      StructDefinition2(
        fullName,
        Conversions.evaluateMutability(struct1.mutability),
        members)

    temputs.add(structDef2);

//    // Lets make a constructor first.
//    // We technically don't have to do it here; it could be made lazily since
//    // the FunctionEnvEntry is already in the env.
//    val _ =
//      OverloadTemplar.scoutExpectedFunctionForPrototype(
//        structInnerEnv,
//        temputs,
//        struct1.name,
//        coercedFinalTemplateArgs,
//        members
//          .map({ case StructMember2(_, _, ReferenceMemberType2(coord)) => coord})
//          .map(coord => ParamFilter(coord, None)),
//        true)

    val implementedInterfaceRefs2 =
      ImplTemplar.getParentInterfaces(temputs, temporaryStructRef);

    implementedInterfaceRefs2.foreach({
      case (implementedInterfaceRef2) => {
        val ownership = if (structDef2.mutability == Mutable) Own else Share
        val (ScoutExpectedFunctionSuccess(_)) =
          OverloadTemplar.scoutExpectedFunctionForPrototype(
            structInnerEnv,
            temputs,
            CallTemplar.INTERFACE_DESTRUCTOR_NAME,
            List(),
            List(ParamFilter(Coord(ownership, structDef2.getRef), Some(Override2(implementedInterfaceRef2)))),
            true)
      }
    })

    val ancestorInterfaces =
      ImplTemplar.getAncestorInterfaces(temputs, temporaryStructRef)

      ancestorInterfaces.foreach({
        case (ancestorInterface) => {
          temputs.addImpl(temporaryStructRef, ancestorInterface)
        }
      })

    (structDef2)
  }

  private def makeStructMembers(env: IEnvironment, temputs: TemputsBox, members: List[StructMemberA]): (List[StructMember2]) = {
    members match {
      case Nil => (Nil)
      case head1 :: tail1 => {
        val head2 = makeStructMember(env, temputs, head1);
        val tail2 = makeStructMembers(env, temputs, tail1);
        (head2 :: tail2)
      }
    }
  }

  private def makeStructMember(
    env: IEnvironment,
    temputs: TemputsBox,
    member: StructMemberA):
  (StructMember2) = {
    val CoordTemplata(coord) = vassertSome(env.getNearestTemplataWithName(member.typeRune, Set(TemplataLookupContext)))
    (StructMember2(member.name, Conversions.evaluateVariability(member.variability), ReferenceMemberType2(coord)))
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    env: IEnvironment,
    temputs: TemputsBox,
    functionS: FunctionA,
    functionFullName: FullName2,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    val mutability =
      getCompoundTypeMutability(temputs, members.map(_.tyype.reference))

    val nearName = FunctionScout.CLOSURE_STRUCT_NAME + functionS.name // For example "__Closure<main>:lam1"
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

    temputs.declareStruct(structRef);
    temputs.declareStructMutability(structRef, mutability)
    temputs.declareStructEnv(structRef, structEnv);

    val closureStructDefinition = StructDefinition2(fullName, mutability, members);
    temputs.add(closureStructDefinition)

    val closuredVarsStructRef = closureStructDefinition.getRef;

    (closuredVarsStructRef, mutability, functionTemplata)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(
    env: NamespaceEnvironment,
    temputs: TemputsBox,
    memberCoords: List[Coord],
    prefix: String):
  (StructRef2, Mutability) = {
    temputs.packTypes.get(memberCoords) match {
      case Some(structRef2) => (structRef2, temputs.lookupStruct(structRef2).mutability)
      case None => {
        val packMutability = getCompoundTypeMutability(temputs, memberCoords)
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

        temputs.declareStruct(newStructDef.getRef);
        temputs.declareStructMutability(newStructDef.getRef, packMutability)
        temputs.declareStructEnv(newStructDef.getRef, env);
        temputs.add(newStructDef)
        temputs.declarePack(memberCoords, newStructDef.getRef);

        (newStructDef.getRef, packMutability)
      }
    }
  }

  def getCompoundTypeMutability(temputs: TemputsBox, memberTypes2: List[Coord])
  : Mutability = {
    val membersOwnerships = memberTypes2.map(_.ownership)
    val allMembersImmutable = membersOwnerships.toSet == Set(Share)
    if (allMembersImmutable) Immutable else Mutable
  }
}
