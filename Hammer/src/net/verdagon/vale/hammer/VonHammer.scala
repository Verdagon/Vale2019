package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.scout.CodeLocationS
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vimpl
import net.verdagon.von._

object VonHammer {
  def vonifyProgram(program: ProgramH): IVonData = {
    val ProgramH(interfaces, structs, emptyPackStructRef, externs, functions) = program

    VonObject(
      "Program",
      None,
      Vector(
        VonMember(None, Some("interfaces"), VonArray(None, interfaces.map(vonfiyInterface).toVector)),
        VonMember(None, Some("structs"), VonArray(None, structs.map(vonfiyStruct).toVector)),
        VonMember(None, Some("emptyPackStructRef"), vonifyStructRef(emptyPackStructRef)),
        VonMember(None, Some("externs"), VonArray(None, externs.map(vonifyPrototype).toVector)),
        VonMember(None, Some("functions"), VonArray(None, functions.map(vonifyFunction).toVector))))
  }

  def vonifyStructRef(ref: StructRefH): IVonData = {
    val StructRefH(fullName) = ref

    VonObject(
      "StructId",
      None,
      Vector(
        VonMember(None, Some("fullName"), fullName.toVonArray())))
  }

  def vonifyInterfaceRef(ref: InterfaceRefH): IVonData = {
    val InterfaceRefH(fullName) = ref

    VonObject(
      "InterfaceId",
      None,
      Vector(
        VonMember(None, Some("fullName"), fullName.toVonArray())))
  }

  def vonfiyInterface(interface: InterfaceDefinitionH): IVonData = {
    val InterfaceDefinitionH(fullName, mutability, superInterfaces, prototypes) = interface

    VonObject(
      "Interface",
      None,
      Vector(
        VonMember(None, Some("fullName"), fullName.toVonArray()),
        VonMember(None, Some("mutability"), vonifyMutability(mutability)),
        VonMember(None, Some("superInterfaces"), VonArray(None, superInterfaces.map(vonifyInterfaceRef).toVector)),
        VonMember(None, Some("methods"), VonArray(None, prototypes.map(vonifyPrototype).toVector))))
  }

  def vonfiyStruct(struct: StructDefinitionH): IVonData = {
    val StructDefinitionH(fullName, mutability, edges, members) = struct

    VonObject(
      "Struct",
      None,
      Vector(
        VonMember(None, Some("fullName"), fullName.toVonArray()),
        VonMember(None, Some("mutability"), vonifyMutability(mutability)),
        VonMember(None, Some("edges"), VonArray(None, edges.map(edge => vonifyEdge(edge)).toVector)),
        VonMember(None, Some("members"), VonArray(None, members.map(vonifyStructMember).toVector))))
  }

  def vonifyMutability(mutability: m.Mutability): IVonData = {
    mutability match {
      case m.Immutable => VonObject("Immutable", None, Vector())
      case m.Mutable => VonObject("Mutable", None, Vector())
    }
  }

  def vonifyPermission(permission: m.Permission): IVonData = {
    permission match {
      case m.Readonly => VonObject("Readonly", None, Vector())
      case m.Readwrite => VonObject("Readwrite", None, Vector())
      case m.ExclusiveReadwrite => VonObject("ExclusiveReadwrite", None, Vector())
    }
  }

  def vonifyLocation(location: m.Location): IVonData = {
    location match {
      case m.Inline => VonObject("Inline", None, Vector())
      case m.Yonder => VonObject("Yonder", None, Vector())
    }
  }

  def vonifyVariability(variability: m.Variability): IVonData = {
    variability match {
      case m.Varying => VonObject("Varying", None, Vector())
      case m.Final => VonObject("Final", None, Vector())
    }
  }

  def vonifyPrototype(prototype: PrototypeH): IVonData = {
    val PrototypeH(fullName, params, returnType) = prototype

    VonObject(
      "Prototype",
      None,
      Vector(
        VonMember(None, Some("fullName"), fullName.toVonArray()),
        VonMember(None, Some("params"), VonArray(None, params.map(vonifyCoord).toVector)),
        VonMember(None, Some("returnType"), vonifyCoord(returnType))))
  }

  def vonifyCoord(coord: ReferenceH[ReferendH]): IVonData = {
    val ReferenceH(ownership, kind) = coord

    VonObject(
      "Ref",
      None,
      Vector(
        VonMember(None, Some("ownership"), vonifyOwnership(ownership)),
        VonMember(None, Some("kind"), vonifyKind(kind))))
  }

  def vonifyEdge(edgeH: EdgeH): IVonData = {
    val EdgeH(struct, interface, structPrototypesByInterfacePrototype) = edgeH

    VonObject(
      "Edge",
      None,
      Vector(
        VonMember(None, Some("struct"), vonifyStructRef(struct)),
        VonMember(None, Some("interface"), vonifyInterfaceRef(interface)),
        VonMember(
          None,
          Some("methods"),
          VonListMap(
            None,
            structPrototypesByInterfacePrototype.toVector.map({ case (interfacePrototype, structPrototype) =>
              VonMapEntry(
                vonifyPrototype(interfacePrototype),
                vonifyPrototype(structPrototype))
            })))))
  }

  def vonifyOwnership(ownership: m.Ownership): IVonData = {
    ownership match {
      case m.Own => VonObject("Own", None, Vector())
      case m.Borrow => VonObject("Borrow", None, Vector())
      case m.Share => VonObject("Share", None, Vector())
    }
  }

  def vonifyStructMember(structMemberH: StructMemberH): IVonData = {
    val StructMemberH(name, variability, tyype) = structMemberH

    VonObject(
      "StructMember",
      None,
      Vector(
        VonMember(None, Some("name"), name.toVonArray()),
        VonMember(None, Some("variability"), vonifyVariability(variability)),
        VonMember(None, Some("type"), vonifyCoord(tyype))))
  }

  def vonifyKind(referend: ReferendH): IVonData = {
    referend match {
      case IntH() => VonObject("Int", None, Vector())
      case BoolH() => VonObject("Bool", None, Vector())
      case StrH() => VonObject("Str", None, Vector())
      case VoidH() => VonObject("Void", None, Vector())
      case FloatH() => VonObject("Float", None, Vector())
      case ir @ InterfaceRefH(_) => vonifyInterfaceRef(ir)
      case sr @ StructRefH(_) => vonifyStructRef(sr)
      case UnknownSizeArrayTH(rawArray) => {
        VonObject(
          "UnknownSizeArray",
          None,
          Vector(
            VonMember(None, Some("array"), vonifyRawArray(rawArray))))
      }
      case KnownSizeArrayTH(size, rawArray) => {
        VonObject(
          "UnknownSizeArray",
          None,
          Vector(
            VonMember(None, Some("size"), VonInt(size)),
            VonMember(None, Some("array"), vonifyRawArray(rawArray))))
      }
    }
  }

  def vonifyRawArray(t: RawArrayTH): IVonData = {
    val RawArrayTH(elementType) = t

    VonObject(
      "Array",
      None,
      Vector(
        VonMember(None, Some("elementType"), vonifyCoord(elementType))))
  }

  def vonifyFunction(functionH: FunctionH): IVonData = {
    val FunctionH(prototype, _, _, _, block) = functionH

    VonObject(
      "Function",
      None,
      Vector(
        VonMember(None, Some("prototype"), vonifyPrototype(prototype)),
        VonMember(None, Some("block"), vonifyBlock(block))))
  }

  def vonifyBlock(block: BlockH): IVonData = {
    val BlockH(nodes, resultType) = block

    VonObject(
      "Function",
      None,
      Vector(
        VonMember(None, Some("nodes"), VonArray(None, nodes.map(node => vonifyNode(node)).toVector)),
        VonMember(None, Some("resultType"), vonifyCoord(resultType))))
  }

  def vonifyNode(node: NodeH): IVonData = {
    node match {
      case ConstantVoidH(registerId) => {
        VonObject(
          "ConstantI64",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId))))
      }
      case ConstantBoolH(registerId, value) => {
        VonObject(
          "ConstantBool",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonBool(value))))
      }
      case ConstantI64H(registerId, value) => {
        VonObject(
          "ConstantI64",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonInt(value))))
      }
      case ConstantStrH(registerId, value) => {
        VonObject(
          "ConstantStr",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonStr(value))))
      }
      case ConstantF64H(registerId, value) => {
        vimpl()
      }
      case DiscardH(registerId, sourceRegister) => {
        VonObject(
          "Discard",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("sourceRegister"), vonifyRegisterAccess(sourceRegister))))
      }
      case ArgumentH(registerId, resultReference, argumentIndex) => {
        VonObject(
          "Argument",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("resultType"), vonifyCoord(resultReference)),
            VonMember(None, Some("argumentIndex"), VonInt(argumentIndex))))
      }
      case StackifyH(registerId, sourceRegister, local, name) => {
        VonObject(
          "Stackify",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("sourceRegister"), vonifyRegisterAccess(sourceRegister)),
            VonMember(None, Some("local"), vonifyLocal(local)),
            VonMember(None, Some("name"), name.toVonArray())))
      }
      case UnstackifyH(registerId, local, expectedType) => {
        VonObject(
          "Unstackify",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("local"), vonifyLocal(local)),
            VonMember(None, Some("expectedType"), vonifyCoord(expectedType))))
      }
      case DestructureH(registerId, structRegister, localTypes, locals) => {
        VonObject(
          "Destructure",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("structRegister"), vonifyRegisterAccess(structRegister)),
            VonMember(
              None,
              Some("localTypes"),
              VonArray(None, localTypes.map(localType => vonifyCoord(localType)).toVector)),
            VonMember(
              None,
              Some("localIndices"),
              VonArray(None, locals.map(local => vonifyLocal(local))))))
      }
      case StructToInterfaceUpcastH(registerId, sourceRegister, targetInterfaceRef) => {
        vimpl()
      }
      case InterfaceToInterfaceUpcastH(registerId, sourceRegister, targetInterfaceRef) => {
        vimpl()
      }
      case LocalStoreH(registerId, local, sourceRegister,localName) => {
        vimpl()
      }
      case LocalLoadH(registerId, local, targetOwnership, expectedLocalType, expectedResultType, localName) => {
        VonObject(
          "LocalLoad",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("local"), vonifyLocal(local)),
            VonMember(None, Some("targetOwnership"), vonifyOwnership(targetOwnership)),
            VonMember(None, Some("expectedLocalType"), vonifyCoord(expectedLocalType)),
            VonMember(None, Some("expectedResultType"), vonifyCoord(expectedResultType)),
            VonMember(None, Some("localName"), localName.toVonArray())))
      }
      case MemberStoreH(registerId, structRegister, memberIndex, sourceRegister, memberName) => {
        vimpl()
      }
      case MemberLoadH(registerId, structRegister, memberIndex, targetOwnership, expectedMemberType, expectedResultType, memberName) => {
        vimpl()
      }
      case KnownSizeArrayStoreH(registerId, arrayRegister, indexRegister, sourceRegister) => {
        vimpl()
      }
      case UnknownSizeArrayStoreH(registerId, arrayRegister, indexRegister, sourceRegister) => {
        vimpl()
      }
      case UnknownSizeArrayLoadH(registerId, arrayRegister, indexRegister, resultType, targetOwnership) => {
        vimpl()
      }
      case KnownSizeArrayLoadH(registerId, arrayRegister, indexRegister, resultType, targetOwnership) => {
        vimpl()
      }
      case CallH(registerId, functionRegister, argsRegisters) => {
        VonObject(
          "ExternCall",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("function"), vonifyPrototype(functionRegister)),
            VonMember(None, Some("argRegisters"), VonArray(None, argsRegisters.toVector.map(vonifyRegisterAccess)))))
      }
      case InterfaceCallH(registerId, argsRegisters, virtualParamIndex, interfaceRefH, indexInEdge, functionType) => {
        vimpl()
      }
      case IfH(registerId, conditionBlock, thenBlock, elseBlock) => {
        vimpl()
      }
      case WhileH(registerId, bodyBlock) => {
        vimpl()
      }
      case InlineBlockH(registerId, block) => {
        VonObject(
          "InlineBlock",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("block"), vonifyBlock(block))))
      }
    }
  }

  def vonifyFunctionRef(ref: FunctionRefH): IVonData = {
    val FunctionRefH(prototype) = ref

    VonObject(
      "FunctionRef",
      None,
      Vector(
        VonMember(None, Some("prototype"), vonifyPrototype(prototype))))
  }

  def vonifyRegisterAccess(access: RegisterAccessH[ReferendH]): IVonData = {
    val RegisterAccessH(registerId, expectedType) = access

    VonObject(
      "RegisterAccess",
      None,
      Vector(
        VonMember(None, Some("registerId"), VonStr(registerId)),
        VonMember(None, Some("expectedType"), vonifyCoord(expectedType))))
  }

  def vonifyLocal(local: Local): IVonData = {
    val Local(id, height, tyype) = local

    VonObject(
      "RegisterAccess",
      None,
      Vector(
        VonMember(None, Some("id"), vonifyVariableId(id)),
        VonMember(None, Some("height"), vonifyStackHeight(height)),
        VonMember(None, Some("type"), vonifyCoord(tyype))))
  }

  def vonifyVariableId(id: VariableIdH): IVonData = {
    val VariableIdH(number, maybeName) = id

    VonObject(
      "VariableId",
      None,
      Vector(
        VonMember(None, Some("number"), VonInt(number)),
        VonMember(
          None,
          Some("name"),
          vonifyOptional[FullNameH](maybeName, x => x.toVonArray()))))
  }

  def vonifyOptional[T](opt: Option[T], func: (T) => IVonData): IVonData = {
    opt match {
      case None => VonObject("None", None, Vector())
      case Some(value) => VonObject("Some", None, Vector(VonMember(None, Some("value"), func(value))))
    }
  }

  def vonifyStackHeight(height: StackHeight): IVonData = {
    val StackHeight(blockHeight, blockStartLocalsHeight, localsHeight) = height

    VonObject(
      "StackHeight",
      None,
      Vector(
        VonMember(None, Some("blockHeight"), VonInt(blockHeight)),
        VonMember(None, Some("blockStartLocalsHeight"), VonInt(blockStartLocalsHeight)),
        VonMember(None, Some("localsHeight"), VonInt(localsHeight))))
  }

//  def translateTemplata(hinputs: Hinputs, hamuts: HamutsBox, templata: ITemplata): ITemplataH = {
//    templata match {
//      case CoordTemplata(reference) => {
//        val (coordH) = TypeHammer.translateReference(hinputs, hamuts, reference)
//        (CoordTemplataH(coordH))
//      }
//      case KindTemplata(kind) => {
//        val (kindH) = TypeHammer.translateKind(hinputs, hamuts, kind)
//        (KindTemplataH(kindH))
//      }
//      case ArrayTemplateTemplata() => ArrayTemplateTemplataH()
//      case FunctionTemplata(outerEnv, unevaluatedContainers, function) => {
//        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
//        (FunctionTemplataH(outerEnvNameH, unevaluatedContainers.map(translateContainer), function.name, Conversions.evaluateCodeLocation(function.codeLocation)))
//      }
//      case StructTemplata(outerEnv, struct) => {
//        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
//        (StructTemplataH(outerEnvNameH, struct.name, Conversions.evaluateCodeLocation(struct.codeLocation)))
//      }
//      case InterfaceTemplata(outerEnv, interface) => {
//        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
//        (InterfaceTemplataH(outerEnvNameH, interface.name, Conversions.evaluateCodeLocation(interface.codeLocation)))
//      }
//      case ImplTemplata(outerEnv, impl) => {
//        val (outerEnvNameH) = translateName(hinputs, hamuts, outerEnv.fullName)
//        (ImplTemplataH(outerEnvNameH, Conversions.evaluateCodeLocation(impl.codeLocation)))
//      }
//      case ExternFunctionTemplata(header) => {
//        val (outerEnvNameH) = translateName(hinputs, hamuts, header.fullName)
//        (ExternFunctionTemplataH(outerEnvNameH))
//      }
//      case OwnershipTemplata(ownership) => OwnershipTemplataH(Conversions.evaluateOwnership(ownership))
//      case VariabilityTemplata(variability) => VariabilityTemplataH(Conversions.evaluateVariability(variability))
//      case MutabilityTemplata(mutability) => MutabilityTemplataH(Conversions.evaluateMutability(mutability))
//      case PermissionTemplata(permission) => PermissionTemplataH(Conversions.evaluatePermission(permission))
//      case LocationTemplata(location) => LocationTemplataH(Conversions.evaluateLocation(location))
//      case BooleanTemplata(value) => BooleanTemplataH(value)
//      case IntegerTemplata(value) => IntegerTemplataH(value)
//    }
//  }

  def vonifyFullName(hinputs: Hinputs, hamuts: HamutsBox, fullName2: FullName2[IName2]): VonStr = {
    val array = VonArray(None, fullName2.steps.map(step => translateName(hinputs, hamuts, step)).toVector)
    VonStr(array.toString)
  }

  def vonifyTemplata(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    templata: ITemplata,
  ): IVonData = {
    templata match {
      case CoordTemplata(coord) => {
        VonObject(
          "CoordTemplata",
          None,
          Vector(
            VonMember(None, Some("coord"), vonifyCoord(TypeHammer.translateReference(hinputs, hamuts, coord)))))
      }
      case KindTemplata(kind) => {
        VonObject(
          "KindTemplata",
          None,
          Vector(
            VonMember(None, Some("kind"), vonifyKind(TypeHammer.translateKind(hinputs, hamuts, kind)))))
      }
      case ArrayTemplateTemplata() => VonObject("ArrayTemplateTemplata", None, Vector())
      case ft @ FunctionTemplata(env, functionA) => {
        VonObject(
          "FunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyFullName(hinputs, hamuts, env.fullName)),
            VonMember(
              None,
              Some("function"),
              translateName(hinputs, hamuts, ft.getTemplateName()))))
      }
      case st @ StructTemplata(env, struct) => {
        VonObject(
          "StructTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyFullName(hinputs, hamuts, env.fullName)),
            VonMember(None, Some("structName"), translateName(hinputs, hamuts, st.getTemplateName()))))
      }
      case it @ InterfaceTemplata(env, interface) => {
        VonObject(
          "InterfaceTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyFullName(hinputs, hamuts, env.fullName)),
            VonMember(None, Some("interfaceName"), translateName(hinputs, hamuts, it.getTemplateName()))))
      }
      case it @ ImplTemplata(env, impl) => {
        VonObject(
          "ExternFunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyFullName(hinputs, hamuts, env.fullName)),
            VonMember(None, Some("location"), translateName(hinputs, hamuts, it.getTemplateName()))))
      }
      case ExternFunctionTemplata(header) => VonObject("ExternFunctionTemplata", None, Vector(VonMember(None, Some("fullName"), vonifyFullName(hinputs, hamuts, header.fullName))))
      case OwnershipTemplata(ownership) => VonObject("OwnershipTemplata", None, Vector(VonMember(None, Some("ownership"), vonifyOwnership(Conversions.evaluateOwnership(ownership)))))
      case VariabilityTemplata(variability) => VonObject("VariabilityTemplata", None, Vector(VonMember(None, Some("variability"), vonifyVariability(Conversions.evaluateVariability(variability)))))
      case MutabilityTemplata(mutability) => VonObject("MutabilityTemplata", None, Vector(VonMember(None, Some("mutability"), vonifyMutability(Conversions.evaluateMutability(mutability)))))
      case PermissionTemplata(permission) => VonObject("PermissionTemplata", None, Vector(VonMember(None, Some("permission"), vonifyPermission(Conversions.evaluatePermission(permission)))))
      case LocationTemplata(location) => VonObject("LocationTemplata", None, Vector(VonMember(None, Some("location"), vonifyLocation(Conversions.evaluateLocation(location)))))
      case BooleanTemplata(value) => VonObject("BooleanTemplata", None, Vector(VonMember(None, Some("value"), VonBool(value))))
      case IntegerTemplata(value) => VonObject("IntegerTemplata", None, Vector(VonMember(None, Some("value"), VonInt(value))))
    }
  }

//  def vonifyContainer(x: ContainerH): IVonData = {
//    val ContainerH(humanName, location) = x
//    VonObject(
//      "Container",
//      None,
//      Vector(
//        VonMember(None, Some(humanName), VonStr(humanName)),
//        VonMember(None, Some("codeLocation"), vonifyCodeLocation(location))))
//  }

  def vonifyCodeLocation(codeLocation: m.CodeLocation): IVonData = {
    val m.CodeLocation(line, char) = codeLocation
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember(None, Some("line"), VonInt(line)),
        VonMember(None, Some("char"), VonInt(char))))
  }

  def vonifyCodeLocationH(codeLocation: CodeLocationH): IVonData = {
    val CodeLocationH(file, line, char) = codeLocation
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember(None, Some("file"), VonStr(file)),
        VonMember(None, Some("line"), VonInt(line)),
        VonMember(None, Some("char"), VonInt(char))))
  }

  def vonifyCodeLocation2(codeLocation: CodeLocation2): IVonData = {
    val CodeLocation2(line, char) = codeLocation
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember(None, Some("line"), VonInt(line)),
        VonMember(None, Some("char"), VonInt(char))))
  }

  def translateName(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    name: IName2
  ): IVonData = {
    name match {
      case ImplDeclareName2(codeLocation) => {
        VonObject(
          "ImplDeclareName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case LetName2(codeLocation) => {
        VonObject(
          "LetName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case TemplarBlockResultVarName2(num) => {
        VonObject(
          "TemplarBlockResultVarName",
          None,
          Vector(
            VonMember(None, Some("num"), VonInt(num))))
      }
      case TemplarFunctionResultVarName2() => {
        VonObject(
          "TemplarFunctionResultVarName",
          None,
          Vector())
      }
      case TemplarTemporaryVarName2(num) => {
        VonObject(
          "TemplarTemporaryVarName",
          None,
          Vector(
            VonMember(None, Some("num"), VonInt(num))))
      }
      case TemplarPatternMemberName2(num, memberIndex) => {
        VonObject(
          "TemplarPatternMemberName",
          None,
          Vector(
            VonMember(None, Some("num"), VonInt(num))))
      }
      case TemplarPatternPackName2(num) => {
        VonObject(
          "TemplarPatternPackName",
          None,
          Vector(
            VonMember(None, Some("num"), VonInt(num))))
      }
      case UnnamedLocalName2(codeLocation) => {
        VonObject(
          "UnnamedLocalName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case ClosureParamName2() => {
        VonObject(
          "ClosureParamName",
          None,
          Vector())
      }
      case MagicParamName2(codeLocation) => {
        VonObject(
          "MagicParamName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case CodeVarName2(name) => {
        VonObject(
          "CodeVarName",
          None,
          Vector(
            VonMember(None, Some("name"), VonStr(name))))
      }
      case AnonymousSubstructMemberName2(index) => {
        VonObject(
          "AnonymousSubstructMemberName",
          None,
          Vector())
      }
      case PrimitiveName2(humanName) => {
        VonObject(
          "PrimitiveName",
          None,
          Vector(
            VonMember(None, Some(humanName), VonStr(humanName))))
      }
      case GlobalNamespaceName2() => {
        VonObject(
          "GlobalNamespaceName",
          None,
          Vector())
      }
      case FunctionName2(humanName, templateArgs, parameters) => {
        VonObject(
          "F",
          None,
          Vector(
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(
              None,
              Some("templateArgs"),
              VonArray(
                None,
                templateArgs
                  .map(templateArg => vonifyTemplata(hinputs, hamuts, templateArg))
                  .toVector)),
            VonMember(
              None,
              Some("parameters"),
              VonArray(
                None,
                parameters
                  .map(templateArg => TypeHammer.translateReference(hinputs, hamuts, templateArg))
                  .map(vonifyCoord)
                  .toVector))))
      }
      case FunctionTemplateName2(humanName, codeLocation) => {
        VonObject(
          "FunctionTemplateName",
          None,
          Vector(
            VonMember(None, Some(humanName), VonStr(humanName)),
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case LambdaTemplateName2(codeLocation) => {
        VonObject(
          "LambdaTemplateName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case ConstructorName2(parameters) => {
        VonObject(
          "ConstructorName",
          None,
          Vector())
      }
      case CitizenName2(humanName, templateArgs) => {
        VonObject(
          "CitizenName",
          None,
          Vector(
            VonMember(None, Some(humanName), VonStr(humanName))))
      }
      case TupleName2(members) => {
        VonObject(
          "TupleName",
          None,
          Vector())
      }
      case LambdaCitizenName2(codeLocation) => {
        VonObject(
          "LambdaCitizenName",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case CitizenTemplateName2(humanName, codeLocation) => {
        VonObject(
          "CitizenTemplateName",
          None,
          Vector(
            VonMember(None, Some(humanName), VonStr(humanName)),
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case AnonymousSubstructName2(callables) => {
        VonObject(
          "AnonymousSubstructName",
          None,
          Vector(
            VonMember(
              None,
              Some("callables"),
              VonArray(
                None,
                callables
                  .map(coord => TypeHammer.translateReference(hinputs, hamuts, coord))
                  .map(vonifyCoord)
                  .toVector))))
      }
      case AnonymousSubstructImplName2() => {
        VonObject(
          "AnonymousSubstructImplName",
          None,
          Vector())
      }
      case CodeRune2(name) => {
        VonObject(
          "CodeRune",
          None,
          Vector(
            VonMember(None, Some("name"), VonStr(name))))
      }
      case ImplicitRune2(name) => {
        VonObject(
          "ImplicitRune",
          None,
          Vector(
            VonMember(None, Some("name"), VonInt(name))))
      }
      case LetImplicitRune2(codeLocation, name) => {
        VonObject(
          "LetImplicitRune",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case MemberRune2(memberIndex) => {
        VonObject(
          "MemberRune",
          None,
          Vector(
            VonMember(None, Some("memberIndex"), VonInt(memberIndex))))
      }
      case MagicImplicitRune2(codeLocation) => {
        VonObject(
          "MagicImplicitRune",
          None,
          Vector(
            VonMember(None, Some("codeLocation"), vonifyCodeLocation2(codeLocation))))
      }
      case ReturnRune2() => {
        VonObject(
          "ReturnRune",
          None,
          Vector())
      }
      case SolverKindRune2(paramRune) => {
        VonObject(
          "SolverKindRune",
          None,
          Vector(
            VonMember(None, Some("paramRune"), translateName(hinputs, hamuts, paramRune))))
      }
      case ExplicitTemplateArgRune2(index) => {
        VonObject(
          "ExplicitTemplateArgRune",
          None,
          Vector(
            VonMember(None, Some("index"), VonInt(index))))
      }
      case AnonymousSubstructParentInterfaceRune2() => {
        VonObject(
          "AnonymousSubstructParentInterfaceRune",
          None,
          Vector())
      }
    }
  }
}
