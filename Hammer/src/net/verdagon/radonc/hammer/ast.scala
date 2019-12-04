package net.verdagon.radonc.hammer

import net.verdagon.radonc.hinputs.TetrisTable
import net.verdagon.radonc.scout.RefCountCategory
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.ListMap

trait IRegister3 {
  def expectReferenceRegister(): ReferenceRegister3 = {
    this match {
      case r @ ReferenceRegister3(_) => r
      case AddressRegister3(_) => vfail("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddressRegister(): AddressRegister3 = {
    this match {
      case a @ AddressRegister3(_) => a
      case ReferenceRegister3(_) => vfail("Expected an address as a result, but got a reference!")
    }
  }
}
case class ReferenceRegister3(reference: Reference3[Referend3]) extends IRegister3
case class AddressRegister3(reference: Reference3[Referend3]) extends IRegister3

sealed trait Node3 {
  def registerId: String;
}

case class RegisterAccess3[+T <: Referend3](
  registerId: String,
  expectedType: Reference3[T]) {

  vassert(expectedType.kind != Void3())

  def expectFunctionAccess(): RegisterAccess3[FunctionT3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ FunctionT3(_, _))) => {
        RegisterAccess3[FunctionT3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectStructAccess(): RegisterAccess3[StructRef3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ StructRef3(_, _))) => {
        RegisterAccess3[StructRef3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectInterfaceAccess(): RegisterAccess3[InterfaceRef3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ InterfaceRef3(_, _))) => {
        RegisterAccess3[InterfaceRef3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectUnknownSizeArrayAccess(): RegisterAccess3[UnknownSizeArrayT3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ UnknownSizeArrayT3(_))) => {
        RegisterAccess3[UnknownSizeArrayT3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectKnownSizeArrayAccess(): RegisterAccess3[KnownSizeArrayT3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ KnownSizeArrayT3(_, _))) => {
        RegisterAccess3[KnownSizeArrayT3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectIntAccess(): RegisterAccess3[Int3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ Int3())) => {
        RegisterAccess3[Int3](registerId, Reference3(ownership, x))
      }
    }
  }
  def expectBoolAccess(): RegisterAccess3[Bool3] = {
    this match {
      case RegisterAccess3(registerId, Reference3(ownership, x @ Bool3())) => {
        RegisterAccess3[Bool3](registerId, Reference3(ownership, x))
      }
    }
  }
}

case class LoadFunction3(
    registerId: String,
    functionRef3: FunctionRef3
) extends Node3 {
  def functionType: FunctionT3 = functionRef3.prototype.functionType
  def resultRef = Reference3(Raw, functionType)
}

case class ConstantVoid3(registerId: String) extends Node3 {
  def resultRef = Reference3(Raw, Void3())
  def resultAccess = RegisterAccess3(registerId, resultRef)
}

case class ConstantBool3(registerId: String, value: Boolean) extends Node3 {
  def resultRef = Reference3(Share, Bool3())
}

case class ConstantI643(registerId: String, value: Int) extends Node3 {
  def resultRef = Reference3(Share, Int3())
  def resultAccess = RegisterAccess3(registerId, resultRef)
}

case class ConstantStr3(registerId: String, value: String) extends Node3 {
  def resultRef = Reference3(Share, Str3())
}

case class ConstantF643(registerId: String, value: Float) extends Node3 {
  def resultRef = Reference3(Share, Float3())
}

case class Argument3(
    registerId: String,
    resultReference: Reference3[Referend3],
    argumentIndex: Int
) extends Node3

case class Stackify3(
    registerId: String,
    sourceRegister: RegisterAccess3[Referend3],
    local: Local,
    name: String
) extends Node3

case class Unstackify3(
    registerId: String,
    local: Local,
    expectedType: Reference3[Referend3]
) extends Node3

case class Destructure3(
    registerId: String,
    structRegister: RegisterAccess3[StructRef3],
    localTypes: List[Reference3[Referend3]],
    localIndices: Vector[Local]
) extends Node3 {
  vassert(localTypes.size == localIndices.size)
}

case class StructToInterfaceUpcast3(
    registerId: String,
    sourceRegister: RegisterAccess3[StructRef3],
    targetInterfaceRef: InterfaceRef3
) extends Node3 {
  def resultRef = Reference3(sourceRegister.expectedType.ownership, targetInterfaceRef)
}

case class InterfaceToInterfaceUpcast3(
    registerId: String,
    sourceRegister: RegisterAccess3[InterfaceRef3],
    targetInterfaceRef: InterfaceRef3
) extends Node3 {
  def resultRef = Reference3(sourceRegister.expectedType.ownership, targetInterfaceRef)
}

case class Reinterpret3(
  registerId: String,
  refRegister: RegisterAccess3[Referend3],
  resultType: Reference3[Referend3],
) extends Node3

case class LocalStore3(
    registerId: String,
    local: Local,
    sourceRegister: RegisterAccess3[Referend3],
    localName: String
) extends Node3

case class LocalLoad3(
    registerId: String,
    local: Local,
    targetOwnership: Ownership,
    expectedLocalType: Reference3[Referend3],
    expectedResultType: Reference3[Referend3],
    localName: String
) extends Node3 {
  vassert(expectedLocalType.kind == expectedResultType.kind)
  vassert(expectedResultType.ownership == targetOwnership)
}

case class MemberStore3(
    registerId: String,
    structRegister: RegisterAccess3[StructRef3],
    memberIndex: Int,
    sourceRegister: RegisterAccess3[Referend3],
    memberName: String
) extends Node3

case class MemberLoad3(
    registerId: String,
    structRegister: RegisterAccess3[StructRef3],
    memberIndex: Int,
    targetOwnership: Ownership,
    expectedMemberType: Reference3[Referend3],
    expectedResultType: Reference3[Referend3],
    memberName: String
) extends Node3 {
  vassert(expectedMemberType.kind == expectedResultType.kind)
  vassert(expectedResultType.ownership == targetOwnership)
}

case class KnownSizeArrayStore3(
  registerId: String,
  arrayRegister: RegisterAccess3[KnownSizeArrayT3],
  indexRegister: RegisterAccess3[Int3],
  sourceRegister: RegisterAccess3[Referend3]
) extends Node3

case class UnknownSizeArrayStore3(
  registerId: String,
  arrayRegister: RegisterAccess3[UnknownSizeArrayT3],
  indexRegister: RegisterAccess3[Int3],
  sourceRegister: RegisterAccess3[Referend3]
) extends Node3

case class UnknownSizeArrayLoad3(
    registerId: String,
    arrayRegister: RegisterAccess3[UnknownSizeArrayT3],
    indexRegister: RegisterAccess3[Int3],
    resultType: Reference3[Referend3],
    targetOwnership: Ownership
) extends Node3

case class KnownSizeArrayLoad3(
    registerId: String,
    arrayRegister: RegisterAccess3[KnownSizeArrayT3],
    indexRegister: RegisterAccess3[Int3],
    resultType: Reference3[Referend3],
    targetOwnership: Ownership
) extends Node3

case class Call3(
    registerId: String,
    functionRegister: RegisterAccess3[FunctionT3],
    argsRegisters: List[RegisterAccess3[Referend3]]
) extends Node3 {
  def resultType = functionRegister.expectedType.kind.returnType
}

case class ExternCall3(
    registerId: String,
    functionRef3: FunctionRef3,
    argsRegisters: List[RegisterAccess3[Referend3]]
) extends Node3

case class InterfaceCall3(
    registerId: String,
    argsRegisters: List[RegisterAccess3[Referend3]],
    virtualParamIndex: Int,
    InterfaceRef3: InterfaceRef3,
    interfaceId: Int,
    indexInEdge: Int,
    functionType: FunctionT3
) extends Node3 {
  vassert(indexInEdge >= 0)
}

case class If3(
    registerId: String,
    conditionBlock: Block3,
    thenBlock: Block3,
    elseBlock: Block3
) extends Node3

case class While3(registerId: String, bodyBlock: Block3) extends Node3

case class InlineBlock3(registerId: String, block: Block3) extends Node3 {

}

case class Block3(
  nodes: Vector[Node3],
  resultType: Reference3[Referend3]
) {
  vassert(nodes.nonEmpty)
  if (nodes.map(_.registerId).toSet.size != nodes.size) {
    vfail("wat")
  }
}

case class Return3(
  registerId: String,
  sourceRegister: RegisterAccess3[Referend3]
) extends Node3


case class ConstructArrayCall3(
    registerId: String,
    sizeRegister: RegisterAccess3[Int3],
    generatorFunctionRegister: RegisterAccess3[FunctionT3],
    generatorArgsRegisters: List[RegisterAccess3[Referend3]],
    arrayRefType: Reference3[UnknownSizeArrayT3]
) extends Node3 {
  vassert(generatorFunctionRegister.expectedType.kind.returnType.kind != Void3())
}

// The consumer function takes just the element
case class DestroyKnownSizeArray3(
  registerId: String,
  arrayRegister: RegisterAccess3[KnownSizeArrayT3],
  consumerFunctionRegister: RegisterAccess3[FunctionT3],
  consumerArgsRegisters: List[RegisterAccess3[Referend3]]
) extends Node3 {
  vassert(consumerFunctionRegister.expectedType.kind.returnType.kind == Void3())
}

// The consumer function takes just the element
case class DestroyUnknownSizeArray3(
  registerId: String,
  arrayRegister: RegisterAccess3[UnknownSizeArrayT3],
  consumerFunctionRegister: RegisterAccess3[FunctionT3],
  consumerArgsRegisters: List[RegisterAccess3[Referend3]],
) extends Node3 {
  vassert(consumerFunctionRegister.expectedType.kind.returnType.kind == Void3())
}

case class Placeholder3(
  registerId: String,
  tyype: Reference3[Referend3]
) extends Node3

case class NewStruct3(
  registerId: String,
  sourceRegisters: List[RegisterAccess3[Referend3]],
  structRefType: Reference3[StructRef3]
) extends Node3

case class ArrayLength3(
  registerId: String,
  sourceRegisters: RegisterAccess3[Referend3],
) extends Node3

case class NewArrayFromValues3(
    registerId: String,
    sourceRegisters: List[RegisterAccess3[Referend3]],
    arrayRefType: Reference3[KnownSizeArrayT3]
) extends Node3

case class CheckRefCount3(
    registerId: String,
    refRegister: RegisterAccess3[Referend3],
    category: RefCountCategory,
    numRegister: RegisterAccess3[Int3]
) extends Node3

case class Discard3(
    registerId: String,
    sourceRegister: RegisterAccess3[Referend3]
) extends Node3

case class NamePart3(humanName: String, maybeTemplateArgs: Option[List[ITemplata3]])
case class FullName3(parts: List[NamePart3])

case class Prototype3(
    functionId: Int,
    fullName: FullName3,
    params: List[Reference3[Referend3]],
    returnType: Reference3[Referend3]
) {
  def functionType = FunctionT3(params, returnType)
}

case class Function3(
    prototype: Prototype3,
    isAbstract: Boolean,
    isExtern: Boolean,
    isUserFunction: Boolean,
    block: Block3) {
  def getRef = FunctionRef3(prototype)
  def fullName = prototype.fullName
}


case class InterfaceDefinition3(
    interfaceId: Int,
    fullName: FullName3,
    mutability: Mutability,
    superInterfaces: List[InterfaceRef3],
    prototypes: List[Prototype3]) {
  def getRef = InterfaceRef3(interfaceId, fullName)
}

case class FunctionRef3(prototype: Prototype3) {
  def functionType = prototype.functionType
  def fullName = prototype.fullName
}

case class Edge3(
    struct: StructRef3,
    interface: InterfaceRef3,
    structPrototypesByInterfacePrototype: ListMap[Prototype3, Prototype3])

case class ETable3(struct: StructRef3, table: TetrisTable[InterfaceRef3, InterfaceRef3])

case class StructMember3(
    name: String,
    variability: Variability,
    tyype: Reference3[Referend3])

case class StructDefinition3(
    structId: Int,
    fullName: FullName3,
    mutability: Mutability,
    eTable: ETable3,
    edges: List[Edge3],
    members: List[StructMember3]) {

  def getRef: StructRef3 = StructRef3(structId, fullName)

  // These functions are tightly coupled with StructSculptor.declareStructInfo
  def getInterfacePtrElementIndex(interfaceRef: InterfaceRef3): Int = {
    val index = edges.indexWhere(_.interface == interfaceRef)
    vassert(index >= 0)
    index
  }
  def getSInfoPtrElementIndex(): Int = {
    edges.size + 1
  }

  def getMemberLlvmIndex(memberIndex: Int): Int = {
    vassert(memberIndex < members.size)
    edges.size + 2 + memberIndex
  }

  def getTypeAndIndex(memberName: String): (Reference3[Referend3], Int) = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => vfail("wat " + this + " " + memberName)
      case Some((member, index)) => (member.tyype, index)
    }
  }
}

case class Program3(
    interfaces: List[InterfaceDefinition3],
    structs: List[StructDefinition3],
    emptyPackStructRef: StructRef3,
    externs: List[Prototype3],
    functions: List[Function3]) {
  def externFunctions = functions.filter(_.isExtern)
  def abstractFunctions = functions.filter(_.isAbstract)
  // Functions that are neither extern nor abstract
  def getAllUserImplementedFunctions = functions.filter(f => f.isUserFunction && !f.isExtern && !f.isAbstract)
  // Abstract or implemented
  def nonExternFunctions = functions.filter(!_.isExtern)
  def getAllUserFunctions = functions.filter(_.isUserFunction)
  def main() = {
    val matching = functions.filter(_.fullName.parts.last.humanName == "main")
    vassert(matching.size == 1)
    matching.head
  }

  def lookupFunction(humanName: String) = {
    val matches = functions.filter(_.fullName.parts.last.humanName == humanName)
    vassert(matches.size == 1)
    matches.head
  }
}

