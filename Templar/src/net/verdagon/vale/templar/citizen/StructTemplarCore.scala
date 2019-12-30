package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser.{FinalP, ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer, FunctionTemplarOrdinaryOrTemplatedLayer}
import net.verdagon.vale._

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
    val fullName = FullName2(List(NamePart2(interface1.name, Some(coercedFinalTemplateArgs2), None, None)))
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

    val internalMethods2 =
      interface1.internalMethods.map(internalMethod => {
        FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForHeader(
          temputs, FunctionTemplata(interfaceInnerEnv, internalMethod))
      })

    val interfaceDef2 =
      InterfaceDefinition2(
        fullName,
        Conversions.evaluateMutability(interface1.mutability),
        internalMethods2)
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
    val fullName = FullName2(List(NamePart2(struct1.name, Some(coercedFinalTemplateArgs), None, None)))
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
        members,
        false)

    temputs.add(structDef2);

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

    structDef2
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

//  // Makes a functor for the given prototype.
//  def functionToLambda(
//    outerEnv: IEnvironment,
//    temputs: TemputsBox,
//    header: FunctionHeader2):
//  StructRef2 = {
//    val mutability = Immutable
//
//    val nearName = FunctionScout.CLOSURE_STRUCT_NAME // For example "__Closure<main>:lam1"
//    val fullName = FullName2(header.fullName.steps :+ NamePart2(nearName, Some(List()), None, None))
//
//    val structRef = StructRef2(fullName)
//
//    // We declare the function into the environment that we use to compile the
//    // struct, so that those who use the struct can reach into its environment
//    // and see the function and use it.
//    // See CSFMSEO and SAFHE.
//    val structEnv =
//      NamespaceEnvironment(
//        Some(outerEnv),
//        fullName,
//        Map(
//          CallTemplar.CALL_FUNCTION_NAME -> List(TemplataEnvEntry(ExternFunctionTemplata(header))),
//          nearName -> List(TemplataEnvEntry(KindTemplata(structRef))),
//          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME -> List(TemplataEnvEntry(KindTemplata(structRef)))))
//
//    temputs.declareStruct(structRef);
//    temputs.declareStructMutability(structRef, mutability)
//    temputs.declareStructEnv(structRef, structEnv);
//
//    val closureStructDefinition = StructDefinition2(fullName, mutability, List(), true);
//    temputs.add(closureStructDefinition)
//
//    val closuredVarsStructRef = closureStructDefinition.getRef;
//
//    closuredVarsStructRef
//  }

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
    val fullName = FullName2(functionFullName.steps :+ NamePart2(nearName, Some(List()), None, None))

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

    val closureStructDefinition = StructDefinition2(fullName, mutability, members, true);
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
            case (pointerType, index) => StructMember2("functor" + index, Final, ReferenceMemberType2(pointerType))
          })

        val memberCoordTemplatas = memberCoords.map(CoordTemplata)
        val templateArgs = memberCoordTemplatas

        val fullName = FullName2(List(NamePart2(prefix, Some(templateArgs), None, None)))

        val newStructDef = StructDefinition2(fullName, packMutability, members, false);
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

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    constructorFullName: FullName2,
    interfaceRef: InterfaceRef2,
    lambdas: List[Coord]):
  (StructRef2, Mutability, FunctionHeader2) = {
    val interfaceDef = temputs.lookupInterface(interfaceRef)

    val mutability = getCompoundTypeMutability(temputs, lambdas)

    val nearName = FunctionScout.ANONYMOUS_SUBSTRUCT_NAME
    val structFullName = FullName2(constructorFullName.steps :+ NamePart2(nearName, Some(KindTemplata(interfaceRef) :: lambdas.map(CoordTemplata)), None, None))

    val structRef = StructRef2(structFullName)

    val forwarderFunctionHeaders =
      interfaceDef.internalMethods.map({ case FunctionHeader2(superFullName, _, _, _, superParams, superReturnType, _) =>
        val params =
          superParams.map({
            case Parameter2(name, Some(Abstract2), Coord(ownership, ir)) => {
              vassert(ir == interfaceRef)
              Parameter2(name, Some(Override2(interfaceRef)), Coord(ownership, structRef))
            }
            case otherParam => otherParam
          })

        val forwarderHeader =
          FunctionHeader2(
            FullName2(
              structFullName.steps :+
                NamePart2(superFullName.steps.last.humanName, Some(List()), Some(params.map(_.tyype)), None)),
            0,
            false,
            false,
            params,
            superReturnType,
            None)

        temputs.declareFunctionSignature(forwarderHeader.toSignature, None)
        forwarderHeader
      })

    val structInnerEnvEntries =
      forwarderFunctionHeaders
        .map(header => {
          (header.fullName.steps.last.humanName -> TemplataEnvEntry(ExternFunctionTemplata(header)))
        })
        .groupBy(_._1)
        .mapValues(_.map(_._2)) ++
      Map(
        // This is used later by the interface constructor generator to know what interface to impl.
        StructTemplar.anonymousSubstructParentInterfaceRune -> List(TemplataEnvEntry(KindTemplata(interfaceRef))),
        Templar.IMPL_NAME -> List(TemplataEnvEntry(ExternImplTemplata(structRef, interfaceRef))))
    val structInnerEnv =
      NamespaceEnvironment(
        Some(outerEnv),
        structFullName,
        structInnerEnvEntries)


    temputs.addImpl(structRef, interfaceRef)

    temputs.declareStruct(structRef)
    temputs.declareStructMutability(structRef, mutability)
    temputs.declareStructEnv(structRef, structInnerEnv)

    vassert(interfaceDef.internalMethods.size == lambdas.size)

    val structDef =
      StructDefinition2(
        structFullName,
        mutability,
        lambdas.zipWithIndex.map({ case (lambda, index) =>
          StructMember2("functor" + index, Final, ReferenceMemberType2(lambda))
        }),
        false)
    temputs.add(structDef)

    forwarderFunctionHeaders.zip(lambdas).foreach({ case (forwarderHeader, lambda) =>
      val localVariables =
        forwarderHeader.params.map(param => {
          ReferenceLocalVariable2(VariableId2(0, param.name), Final, param.tyype)
        })

      // The args for the call inside the forwarding function.
      val forwardedCallArgs =
        (Coord(Borrow, lambda.referend) :: forwarderHeader.paramTypes.tail).map(ParamFilter(_, None))

      val lambdaFunctionPrototype =
        OverloadTemplar.scoutExpectedFunctionForPrototype(
          outerEnv,
          temputs,
          CallTemplar.CALL_FUNCTION_NAME,
          List(),
          forwardedCallArgs,
          true) match {
            case seff @ ScoutExpectedFunctionFailure(_, _, _, _, _) => vfail(seff.toString)
            case ScoutExpectedFunctionSuccess(prototype) => prototype
        }

      val argExpressions =
        SoftLoad2(
          ReferenceMemberLookup2(
            ArgLookup2(0, Coord(Own, structRef)),
            "this",
            Coord(Own, lambda.referend)),
          Borrow) ::
        forwarderHeader.params.tail.zipWithIndex.map({ case (param, index) =>
          ArgLookup2(index, param.tyype)
        })

      val forwarderFunction =
        Function2(
          forwarderHeader,
          localVariables,
          Block2(
            List(
              FunctionCall2(lambdaFunctionPrototype, argExpressions))))
      temputs.addFunction(forwarderFunction)
    })

    val constructor = makeStructConstructor(temputs, maybeConstructorOriginFunctionA, structDef, constructorFullName)

    (structRef, mutability, constructor)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  def prototypeToAnonymousSubstruct(
    outerEnv: IEnvironment,
    temputs: TemputsBox,
    interfaceRef: InterfaceRef2,
    prototype: Prototype2):
  StructRef2 = {
    val interfaceDef = temputs.lookupInterface(interfaceRef)

    val mutability = Immutable

    // This is saying the function's full name, and then as a next step, the interface ref.
    // We could do it the other way, but theres no good way to put a Prototype2 into a
    // templata which can be in the name. We could change it someday if we have a Prototype2
    // as a templata...
    val nearName = FunctionScout.ANONYMOUS_SUBSTRUCT_NAME
    val structFullName =
      FullName2(
        prototype.fullName.steps :+
          NamePart2(nearName, Some(List(KindTemplata(interfaceRef))), None, None))

    val structRef = StructRef2(structFullName)

    vassert(interfaceDef.internalMethods.size == 1)
    val List(FunctionHeader2(superFullName, _, _, _, superParams, superReturnType, _)) = interfaceDef.internalMethods

    val params =
      superParams.map({
        case Parameter2(name, Some(Abstract2), Coord(ownership, ir)) => {
          vassert(ir == interfaceRef)
          Parameter2(name, Some(Override2(interfaceRef)), Coord(ownership, structRef))
        }
        case otherParam => otherParam
      })

    val forwarderHeader =
      FunctionHeader2(
        FullName2(
          structFullName.steps :+
            NamePart2(superFullName.steps.last.humanName, Some(List()), Some(params.map(_.tyype)), None)),
        0,
        false,
        false,
        params,
        superReturnType,
        None)

    temputs.declareFunctionSignature(forwarderHeader.toSignature, None)


    val structInnerEnvEntries =
      Map(
          forwarderHeader.fullName.steps.last.humanName -> List(TemplataEnvEntry(ExternFunctionTemplata(forwarderHeader))),
          // This is used later by the interface constructor generator to know what interface to impl.
          StructTemplar.anonymousSubstructParentInterfaceRune -> List(TemplataEnvEntry(KindTemplata(interfaceRef))),
          Templar.IMPL_NAME -> List(TemplataEnvEntry(ExternImplTemplata(structRef, interfaceRef))))
    val structInnerEnv =
      NamespaceEnvironment(
        Some(outerEnv),
        structFullName,
        structInnerEnvEntries)

    temputs.addImpl(structRef, interfaceRef)

    temputs.declareStruct(structRef)
    temputs.declareStructMutability(structRef, mutability)
    temputs.declareStructEnv(structRef, structInnerEnv)

    val structDef =
      StructDefinition2(
        structFullName,
        mutability,
        List(),
        false)
    temputs.add(structDef)

    val argExpressions =
        forwarderHeader.params.tail.zipWithIndex.map({ case (param, index) =>
          ArgLookup2(index, param.tyype)
        })

    val forwarderFunction =
      Function2(
        forwarderHeader,
        List(),
        Block2(
          List(
            FunctionCall2(prototype, argExpressions))))
    temputs.addFunction(forwarderFunction)

    // Dont need to make a constructor because the only user of this function currently
    // just constructs the struct directly... easy because it's empty.

    structRef
  }

  def makeStructConstructor(
    temputs: TemputsBox,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    structDef: StructDefinition2,
    constructorFullName: FullName2):
  FunctionHeader2 = {
    val constructorParams =
      structDef.members.map({
        case StructMember2(name, _, ReferenceMemberType2(reference)) => {
          Parameter2(name, None, reference)
        }
      })
    val constructorReturnOwnership = if (structDef.mutability == Mutable) Own else Share
    val constructorReturnType = Coord(constructorReturnOwnership, structDef.getRef)
    // not virtual because how could a constructor be virtual
    val constructor2 =
      Function2(
        FunctionHeader2(
          constructorFullName,
          0,
          false, false,
          constructorParams,
          constructorReturnType,
          maybeConstructorOriginFunctionA),
        List(),
        Block2(
          List(
            Construct2(
              structDef.getRef,
              Coord(if (structDef.mutability == Mutable) Own else Share, structDef.getRef),
              constructorParams.zipWithIndex.map({ case (p, index) => ArgLookup2(index, p.tyype) })))))

    // we cant make the destructor here because they might have a user defined one somewhere

    temputs
      .declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
    temputs.addFunction(constructor2);

    vassert(temputs.exactDeclaredSignatureExists(constructor2.header.fullName, constructor2.header.toBanner.paramTypes))

    (constructor2.header)
  }
}
