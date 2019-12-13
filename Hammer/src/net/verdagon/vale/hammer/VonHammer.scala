package net.verdagon.vale.hammer

import net.verdagon.vale.scout.CodeLocation
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vimpl
import net.verdagon.von._

object VonHammer {
  def vonifyProgram(program: Program3): IVonData = {
    val Program3(interfaces, structs, emptyPackStructRef, externs, functions) = program

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

  def vonifyStructRef(ref: StructRef3): IVonData = {
    val StructRef3(_, fullName) = ref

    VonObject(
      "StructId",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName))))
  }

  def vonifyInterfaceRef(ref: InterfaceRef3): IVonData = {
    val InterfaceRef3(_, fullName) = ref

    VonObject(
      "InterfaceId",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName))))
  }

  def vonfiyInterface(interface: InterfaceDefinition3): IVonData = {
    val InterfaceDefinition3(_, fullName, mutability, superInterfaces, prototypes) = interface

    VonObject(
      "Interface",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName)),
        VonMember(None, Some("mutability"), vonifyMutability(mutability)),
        VonMember(None, Some("superInterfaces"), VonArray(None, superInterfaces.map(vonifyInterfaceRef).toVector)),
        VonMember(None, Some("methods"), VonArray(None, prototypes.map(vonifyPrototype).toVector))))
  }

  def vonfiyStruct(struct: StructDefinition3): IVonData = {
    val StructDefinition3(_, fullName, mutability, _, edges, members) = struct

    VonObject(
      "Struct",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName)),
        VonMember(None, Some("mutability"), vonifyMutability(mutability)),
        VonMember(None, Some("edges"), VonArray(None, edges.map(vonifyEdge).toVector)),
        VonMember(None, Some("members"), VonArray(None, members.map(vonifyStructMember).toVector))))
  }

  def vonifyMutability(mutability: Mutability): IVonData = {
    mutability match {
      case Immutable => VonObject("Immutable", None, Vector())
      case Mutable => VonObject("Mutable", None, Vector())
    }
  }

  def vonifyPermission(permission: Permission): IVonData = {
    permission match {
      case Readonly => VonObject("Readonly", None, Vector())
      case Readwrite => VonObject("Readwrite", None, Vector())
      case ExclusiveReadwrite => VonObject("ExclusiveReadwrite", None, Vector())
    }
  }

  def vonifyLocation(location: Location): IVonData = {
    location match {
      case Inline => VonObject("Inline", None, Vector())
      case Yonder => VonObject("Yonder", None, Vector())
    }
  }

  def vonifyVariability(variability: Variability): IVonData = {
    variability match {
      case Varying => VonObject("Varying", None, Vector())
      case Final => VonObject("Final", None, Vector())
    }
  }

  def vonifyPrototype(prototype: Prototype3): IVonData = {
    val Prototype3(_, fullName, params, returnType) = prototype

    VonObject(
      "Prototype",
      None,
      Vector(
        VonMember(None, Some("fullName"), vonifyName(fullName)),
        VonMember(None, Some("params"), VonArray(None, params.map(vonifyCoord).toVector)),
        VonMember(None, Some("returnType"), vonifyCoord(returnType))))
  }

  def vonifyCoord(coord: Reference3[Referend3]): IVonData = {
    val Reference3(ownership, kind) = coord

    VonObject(
      "Reference",
      None,
      Vector(
        VonMember(None, Some("ownership"), vonifyOwnership(ownership)),
        VonMember(None, Some("kind"), vonifyKind(kind))))
  }

  def vonifyEdge(edge3: Edge3): IVonData = {
    val Edge3(struct, interface, structPrototypesByInterfacePrototype) = edge3

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

  def vonifyOwnership(ownership: Ownership): IVonData = {
    ownership match {
      case Raw => VonObject("Raw", None, Vector())
      case Own => VonObject("Own", None, Vector())
      case Borrow => VonObject("Borrow", None, Vector())
      case Share => VonObject("Share", None, Vector())
    }
  }

  def vonifyStructMember(structMember3: StructMember3): IVonData = {
    val StructMember3(name, variability, tyype) = structMember3

    VonObject(
      "StructMember",
      None,
      Vector(
        VonMember(None, Some("name"), VonStr(name)),
        VonMember(None, Some("variability"), vonifyVariability(variability)),
        VonMember(None, Some("type"), vonifyCoord(tyype))))
  }

  def vonifyKind(referend: Referend3): IVonData = {
    referend match {
      case Int3() => VonObject("Int3", None, Vector())
      case Bool3() => VonObject("Bool3", None, Vector())
      case Str3() => VonObject("Str3", None, Vector())
      case Void3() => VonObject("Void3", None, Vector())
      case Float3() => VonObject("Float3", None, Vector())
      case ir @ InterfaceRef3(_, _) => vonifyInterfaceRef(ir)
      case sr @ StructRef3(_, _) => vonifyStructRef(sr)
      case FunctionT3(paramTypes, returnType) => {
        VonObject(
          "FunctionT3",
          None,
          Vector(
            VonMember(
              None,
              Some("paramTypes"),
              VonArray(None, paramTypes.map(vonifyCoord).toVector)),
            VonMember(None, Some("returnType"), vonifyCoord(returnType))))
      }
      case UnknownSizeArrayT3(rawArray) => {
        VonObject(
          "UnknownSizeArray",
          None,
          Vector(
            VonMember(None, Some("array"), vonifyRawArray(rawArray))))
      }
      case KnownSizeArrayT3(size, rawArray) => {
        VonObject(
          "UnknownSizeArray",
          None,
          Vector(
            VonMember(None, Some("size"), VonInt(size)),
            VonMember(None, Some("array"), vonifyRawArray(rawArray))))
      }
    }
  }

  def vonifyRawArray(t: RawArrayT3): IVonData = {
    val RawArrayT3(elementType) = t

    vimpl() // this needs a mutability doesnt it?
    VonObject(
      "Array",
      None,
      Vector(
        VonMember(None, Some("elementType"), vonifyCoord(elementType))))
  }

  def vonifyFunction(function3: Function3): IVonData = {
    val Function3(prototype, _, _, _, block) = function3

    VonObject(
      "Function",
      None,
      Vector(
        VonMember(None, Some("prototype"), vonifyPrototype(prototype)),
        VonMember(None, Some("block"), vonifyBlock(block))))
  }

  def vonifyBlock(block: Block3): IVonData = {
    val Block3(nodes, resultType) = block

    VonObject(
      "Function",
      None,
      Vector(
        VonMember(None, Some("nodes"), VonArray(None, nodes.map(vonifyNode).toVector)),
        VonMember(None, Some("resultType"), vonifyCoord(resultType))))
  }

  def vonifyNode(node: Node3): IVonData = {
    node match {
      case LoadFunction3(registerId, functionRef3) => {
        VonObject(
          "LoadFunction",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("functionRef"), vonifyFunctionRef(functionRef3))))
      }
      case ConstantVoid3(registerId) => {
        VonObject(
          "ConstantI64",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId))))
      }
      case ConstantBool3(registerId, value) => {
        VonObject(
          "ConstantBool",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonBool(value))))
      }
      case ConstantI643(registerId, value) => {
        VonObject(
          "ConstantI64",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonInt(value))))
      }
      case ConstantStr3(registerId, value) => {
        VonObject(
          "ConstantStr",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("value"), VonStr(value))))
      }
      case ConstantF643(registerId, value) => {
        vimpl()
      }
      case Discard3(registerId, sourceRegister) => {
        VonObject(
          "Discard",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("sourceRegister"), vonifyRegisterAccess(sourceRegister))))
      }
      case Argument3(registerId, resultReference, argumentIndex) => {
        VonObject(
          "Argument",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("resultType"), vonifyCoord(resultReference)),
            VonMember(None, Some("argumentIndex"), VonInt(argumentIndex))))
      }
      case Stackify3(registerId, sourceRegister, local, name) => {
        VonObject(
          "Stackify",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("sourceRegister"), vonifyRegisterAccess(sourceRegister)),
            VonMember(None, Some("local"), vonifyLocal(local)),
            VonMember(None, Some("name"), VonStr(name))))
      }
      case Unstackify3(registerId, local, expectedType) => {
        VonObject(
          "Unstackify",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("local"), vonifyLocal(local)),
            VonMember(None, Some("expectedType"), vonifyCoord(expectedType))))
      }
      case Destructure3(registerId, structRegister, localTypes, locals) => {
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
      case StructToInterfaceUpcast3(registerId, sourceRegister, targetInterfaceRef) => {
        vimpl()
      }
      case InterfaceToInterfaceUpcast3(registerId, sourceRegister, targetInterfaceRef) => {
        vimpl()
      }
      case LocalStore3(registerId, local, sourceRegister,localName) => {
        vimpl()
      }
      case LocalLoad3(registerId, local, targetOwnership, expectedLocalType, expectedResultType, localName) => {
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
      case MemberStore3(registerId, structRegister, memberIndex, sourceRegister, memberName) => {
        vimpl()
      }
      case MemberLoad3(registerId, structRegister, memberIndex, targetOwnership, expectedMemberType, expectedResultType, memberName) => {
        vimpl()
      }
      case KnownSizeArrayStore3(registerId, arrayRegister, indexRegister, sourceRegister) => {
        vimpl()
      }
      case UnknownSizeArrayStore3(registerId, arrayRegister, indexRegister, sourceRegister) => {
        vimpl()
      }
      case UnknownSizeArrayLoad3(registerId, arrayRegister, indexRegister, resultType, targetOwnership) => {
        vimpl()
      }
      case KnownSizeArrayLoad3(registerId, arrayRegister, indexRegister, resultType, targetOwnership) => {
        vimpl()
      }
      case Call3(registerId, functionRegister, argsRegisters) => {
        VonObject(
          "ExternCall",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("functionRegister"), vonifyRegisterAccess(functionRegister)),
            VonMember(None, Some("argRegisters"), VonArray(None, argsRegisters.toVector.map(vonifyRegisterAccess)))))
      }
      case ExternCall3(registerId, functionRef3, argsRegisters) => {
        VonObject(
          "ExternCall",
          None,
          Vector(
            VonMember(None, Some("registerId"), VonStr(registerId)),
            VonMember(None, Some("functionRef"), vonifyFunctionRef(functionRef3)),
            VonMember(None, Some("argRegisters"), VonArray(None, argsRegisters.toVector.map(vonifyRegisterAccess)))))
      }
      case InterfaceCall3(registerId, argsRegisters, virtualParamIndex, interfaceRef3, interfaceId, indexInEdge, functionType) => {
        vimpl()
      }
      case If3(registerId, conditionBlock, thenBlock, elseBlock) => {
        vimpl()
      }
      case While3(registerId, bodyBlock) => {
        vimpl()
      }
      case InlineBlock3(registerId, block) => {
        vimpl()
      }
    }
  }

  def vonifyFunctionRef(ref: FunctionRef3): IVonData = {
    val FunctionRef3(prototype) = ref

    VonObject(
      "FunctionRef",
      None,
      Vector(
        VonMember(None, Some("prototype"), vonifyPrototype(prototype))))
  }

  def vonifyRegisterAccess(access: RegisterAccess3[Referend3]): IVonData = {
    val RegisterAccess3(registerId, expectedType) = access

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

  def vonifyVariableId(id: VariableId3): IVonData = {
    val VariableId3(number, maybeName) = id

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

  def vonifyName(name: FullName3): IVonData = {
    VonObject(
      "FullName",
      None,
      Vector(
        VonMember(
          None,
          Some("parts"),
          VonArray(
            None,
            name.parts.map({ case NamePart3(humanName, maybeTemplateArgs) =>
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

  def vonifyTemplata(templata: ITemplata3): IVonData = {
    templata match {
      case CoordTemplata3(coord) => {
        VonObject(
          "CoordTemplata",
          None,
          Vector(
            VonMember(None, Some("coord"), vonifyCoord(coord))))
      }
      case KindTemplata3(kind) => {
        VonObject(
          "KindTemplata",
          None,
          Vector(
            VonMember(None, Some("kind"), vonifyKind(kind))))
      }
      case ArrayTemplateTemplata3() => VonObject("ArrayTemplateTemplata", None, Vector())
      case FunctionTemplata3(envName, humanName, location) => {
        VonObject(
          "FunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case StructTemplata3(envName, humanName, location) => {
        VonObject(
          "StructTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case InterfaceTemplata3(envName, humanName, location) => {
        VonObject(
          "InterfaceTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("humanName"), VonStr(humanName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case ImplTemplata3(envName, location) => {
        VonObject(
          "ExternFunctionTemplata",
          None,
          Vector(
            VonMember(None, Some("envName"), vonifyName(envName)),
            VonMember(None, Some("location"), vonifyCodeLocation(location))))
      }
      case ExternFunctionTemplata3(fullName) => VonObject("ExternFunctionTemplata", None, Vector(VonMember(None, Some("fullName"), vonifyName(fullName))))
      case OwnershipTemplata3(ownership) => VonObject("OwnershipTemplata", None, Vector(VonMember(None, Some("ownership"), vonifyOwnership(ownership))))
      case VariabilityTemplata3(variability) => VonObject("VariabilityTemplata", None, Vector(VonMember(None, Some("variability"), vonifyVariability(variability))))
      case MutabilityTemplata3(mutability) => VonObject("MutabilityTemplata", None, Vector(VonMember(None, Some("mutability"), vonifyMutability(mutability))))
      case PermissionTemplata3(permission) => VonObject("PermissionTemplata", None, Vector(VonMember(None, Some("permission"), vonifyPermission(permission))))
      case LocationTemplata3(location) => VonObject("LocationTemplata", None, Vector(VonMember(None, Some("location"), vonifyLocation(location))))
      case BooleanTemplata3(value) => VonObject("BooleanTemplata", None, Vector(VonMember(None, Some("value"), VonBool(value))))
      case IntegerTemplata3(value) => VonObject("IntegerTemplata", None, Vector(VonMember(None, Some("value"), VonInt(value))))
    }
  }

  def vonifyCodeLocation(codeLocation: CodeLocation): IVonData = {
    val CodeLocation(file, line, char) = codeLocation
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember(None, Some("file"), VonStr(file)),
        VonMember(None, Some("line"), VonInt(line)),
        VonMember(None, Some("char"), VonInt(char))))
  }
}
