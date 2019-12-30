package net.verdagon.vale.hammer

import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.scout.CodeLocationS
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
        VonMember(None, Some("fullName"), vonifyName(fullName))))
  }

  def vonifyInterfaceRef(ref: InterfaceRefH): IVonData = {
    val InterfaceRefH(fullName) = ref

    VonObject(
      "InterfaceId",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName))))
  }

  def vonfiyInterface(interface: InterfaceDefinitionH): IVonData = {
    val InterfaceDefinitionH(fullName, mutability, superInterfaces, prototypes) = interface

    VonObject(
      "Interface",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName)),
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
        VonMember(None, Some("fullName"), vonifyName(fullName)),
        VonMember(None, Some("mutability"), vonifyMutability(mutability)),
        VonMember(None, Some("edges"), VonArray(None, edges.map(vonifyEdge).toVector)),
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
        VonMember(None, Some("fullName"), vonifyName(fullName)),
        VonMember(None, Some("params"), VonArray(None, params.map(vonifyCoord).toVector)),
        VonMember(None, Some("returnType"), vonifyCoord(returnType))))
  }

  def vonifyCoord(coord: ReferenceH[ReferendH]): IVonData = {
    val ReferenceH(ownership, kind) = coord

    VonObject(
      "Reference",
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
              VonMapEntry(vonifyPrototype(interfacePrototype), vonifyPrototype(structPrototype))
            })))))
  }

  def vonifyOwnership(ownership: m.Ownership): IVonData = {
    ownership match {
      case m.Raw => VonObject("Raw", None, Vector())
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
        VonMember(None, Some("name"), VonStr(name)),
        VonMember(None, Some("variability"), vonifyVariability(variability)),
        VonMember(None, Some("type"), vonifyCoord(tyype))))
  }

  def vonifyKind(referend: ReferendH): IVonData = {
    referend match {
      case IntH() => VonObject("IntH", None, Vector())
      case BoolH() => VonObject("BoolH", None, Vector())
      case StrH() => VonObject("StrH", None, Vector())
      case VoidH() => VonObject("VoidH", None, Vector())
      case FloatH() => VonObject("FloatH", None, Vector())
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

    vimpl() // this needs a mutability doesnt it?
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
        VonMember(None, Some("nodes"), VonArray(None, nodes.map(vonifyNode).toVector)),
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
            VonMember(None, Some("name"), VonStr(name))))
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
            VonMember(None, Some("localName"), VonStr(localName))))
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
        vimpl()
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
        VonMember(None, Some("name"), vonifyOptional(maybeName, VonStr))))
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

  def vonifyName(name: FullNameH): IVonData = {
    VonObject(
      "FullName",
      None,
      Vector(
        VonMember(
          None,
          Some("parts"),
          VonArray(
            None,
            name.parts.map({ case NamePartH(humanName, maybeTemplateArgs) =>
              VonObject(
                "NamePart",
                None,
                Vector(
                  VonMember(None, Some("humanName"), VonStr(humanName)),
                  VonMember(
                    None,
                    Some("maybeTemplateArgs"),
                    maybeTemplateArgs match {
                      case None => VonObject("None", None, Vector())
                      case Some(templateArgs) => {
                        VonObject(
                          "Some",
                          None,
                          Vector(
                            VonMember(None, Some("value"), VonArray(None, templateArgs.map(vonifyTemplata).toVector))))
                      }
                    })))
            }).toVector))))
  }

  def vonifyTemplata(templata: ITemplataH): IVonData = {
    templata match {
      case CoordTemplataH(coord) => {
        VonObject(
          "CoordTemplata",
          None,
          Vector(
            VonMember(None, Some("coord"), vonifyCoord(coord))))
      }
      case KindTemplataH(kind) => {
        VonObject(
          "KindTemplata",
          None,
          Vector(
            VonMember(None, Some("kind"), vonifyKind(kind))))
      }
      case ArrayTemplateTemplataH() => VonObject("ArrayTemplateTemplata", None, Vector())
      case FunctionTemplataH(envName, humanName, location) => {
        VonObject(
          "FunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case StructTemplataH(envName, humanName, location) => {
        VonObject(
          "StructTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case InterfaceTemplataH(envName, humanName, location) => {
        VonObject(
          "InterfaceTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case ImplTemplataH(envName, location) => {
        VonObject(
          "ExternFunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case ExternFunctionTemplataH(fullName) => VonObject("ExternFunctionTemplata", None, Vector(VonMember(None, Some("fullName"), vonifyName(fullName))))
      case OwnershipTemplataH(ownership) => VonObject("OwnershipTemplata", None, Vector(VonMember(None, Some("ownership"), vonifyOwnership(ownership))))
      case VariabilityTemplataH(variability) => VonObject("VariabilityTemplata", None, Vector(VonMember(None, Some("variability"), vonifyVariability(variability))))
      case MutabilityTemplataH(mutability) => VonObject("MutabilityTemplata", None, Vector(VonMember(None, Some("mutability"), vonifyMutability(mutability))))
      case PermissionTemplataH(permission) => VonObject("PermissionTemplata", None, Vector(VonMember(None, Some("permission"), vonifyPermission(permission))))
      case LocationTemplataH(location) => VonObject("LocationTemplata", None, Vector(VonMember(None, Some("location"), vonifyLocation(location))))
      case BooleanTemplataH(value) => VonObject("BooleanTemplata", None, Vector(VonMember(None, Some("value"), VonBool(value))))
      case IntegerTemplataH(value) => VonObject("IntegerTemplata", None, Vector(VonMember(None, Some("value"), VonInt(value))))
    }
  }

  def vonifyCodeLocation(codeLocation: m.CodeLocation): IVonData = {
    val m.CodeLocation(file, line, char) = codeLocation
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember(None, Some("file"), VonStr(file)),
        VonMember(None, Some("line"), VonInt(line)),
        VonMember(None, Some("char"), VonInt(char))))
  }
}
