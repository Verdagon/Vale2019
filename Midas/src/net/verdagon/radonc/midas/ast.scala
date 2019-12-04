package net.verdagon.radonc.midas

import net.verdagon.radonc.carpenter.TetrisTable
import net.verdagon.radonc.hammer._

import scala.collection.immutable.ListMap

trait Node4 {
  def registerId: Int;
  def register: Register4;
}

// This is different in that it uses the name verbatim
case class LoadExternFunction4(registerId: Int, prototype3: Prototype3, functionType: FunctionT4) extends Node4 {
  override def register = ReferenceRegister4(FunctionReference4(functionType))
}
case class LoadFunction4(registerId: Int, prototype3: Prototype3, functionType: FunctionT4) extends Node4 {
  override def register = ReferenceRegister4(FunctionReference4(functionType))
}
case class ConstantBool4(registerId: Int, value: Boolean) extends Node4 {
  override def register = ReferenceRegister4(BoolReference4())
}
case class ConstantI644(registerId: Int, value: Int) extends Node4 {
  override def register = ReferenceRegister4(IntReference4())
}
case class ConstantF644(registerId: Int, value: Float) extends Node4 {
  override def register = ReferenceRegister4(FloatReference4())
}
case class Argument4(registerId: Int, argumentIndex: Int, tyype: Reference4) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}
case class Stack4(registerId: Int, tyype: Reference4, name: String) extends Node4 {
  override def register = VariableAddressRegister4(tyype)
}
case class Malloc4(registerId: Int, tyype: Reference4, name: String) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}
case class Free4(registerId: Int, sourceRegisterId: Int) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class Store4(registerId: Int, tyype: Reference4, destinationRegisterId: Int, valueRegisterId: Int) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class LocalLookup4(registerId: Int, tyype: Reference4, sourceRegisterId: Int, name: String) extends Node4 {
  override def register = VariableAddressRegister4(tyype)
}
case class SoftLoad4(registerId: Int, tyype: Reference4, sourceRegisterId: Int) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}
case class StructToInterfaceUpcast4(
    registerId: Int,
    sourceStructRef: StructId4,
    targetInterfaceRef: InterfaceId4,
    resultType: Reference4,
    sourceRegisterId: Int) extends Node4 {
  override def register = ReferenceRegister4(resultType)
}
case class InterfaceToInterfaceUpcast4(
    registerId: Int,
    sourceInterfaceRef: InterfaceId4,
    targetInterfaceRef: InterfaceId4,
    resultType: Reference4,
    sourceRegisterId: Int) extends Node4 {
  override def register = ReferenceRegister4(resultType)
}
case class InterfaceMethodLookup4(
    registerId: Int,
    interfaceRef4: InterfaceId4,
    interfaceRegisterId: Int,
    interfaceId: Int,
    indexInEdge: Int,
    functionType: FunctionT4) extends Node4 {
  override def register = ReferenceRegister4(FunctionReference4(functionType))
}

case class StructLookup4(registerId: Int, sourceRegisterId: Int, structType: StructId4, memberIndex: Int, memberType: Reference4) extends Node4 {
  override def register = VariableAddressRegister4(memberType)
}
case class ClosureStructLookup4(registerId: Int, sourceRegisterId: Int, structId: StructId4, memberIndex: Int, memberType: Reference4) extends Node4 {
  override def register = VariableAddressRegister4(memberType)
}
case class And4(registerId: Int, leftRegisterId: Int, rightRegisterId: Int) extends Node4 {
  override def register = ReferenceRegister4(BoolReference4())
}
case class Or4(registerId: Int, leftRegisterId: Int, rightRegisterId: Int) extends Node4 {
  override def register = ReferenceRegister4(BoolReference4())
}
case class Call4(registerId: Int, functionRegisterId: Int, argsRegisterIds: List[Int], paramTypes: List[Reference4], returnType: Reference4) extends Node4 {
  override def register = ReferenceRegister4(returnType)
}
case class Return4(registerId: Int, sourceRegisterId: Int, returnType: Reference4) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class Branch4(registerId: Int, conditionRegisterId: Int, trueDestinationRegisterId: Int, falseDestinationRegisterId: Int) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class Jump4(registerId: Int, destinationRegisterId: Int) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class PhiCase4(bodyLabelRegisterId: Int, resultRegisterId: Int)
case class Phi4(registerId: Int, tyype: Reference4, cases: List[PhiCase4]) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}
case class Label4(registerId: Int) extends Node4 {
  override def register: Register4 = vfail("this has no result type")
}
case class NewRegisterStruct4(registerId: Int, tyype: InlineStructReference4, sourceRegisterIds: List[Int]) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}
case class NewHeapStruct4(registerId: Int, tyype: FarStructReference4, sourceRegisterIds: List[Int]) extends Node4 {
  override def register = ReferenceRegister4(tyype)
}

case class Prototype4(hamut: Prototype3, humanName: String, params: List[Reference4], returnType: Reference4) {
  def functionType() = FunctionT4(hamut.functionType, params, returnType)
}

case class Function4(
    origin: Function3,
    prototype: Prototype4,
    isAbstract: Boolean,
    isExtern: Boolean,
    nodes: Array[Node4]) {
  def getRef = FunctionRef4(origin.getRef, prototype)
}

case class FunctionRef4(origin: FunctionRef3, prototype: Prototype4)


case class StructDefinition4(
    origin: StructDefinition3,
    base: Option[StructId4],
    eTable: ETable4,
    edges: List[Edge4],
    members: List[StructMember4],
    methods: List[FunctionRef4],
    size: Int,
    align: Int) {
  def humanName = origin.humanName
  def mutability = origin.mutability
  def getRef = StructId4(origin.getRef)

  // These functions are tightly coupled with StructSculptor.declareStructInfo
  def getInterfacePtrElementIndex(interfaceRef: InterfaceId4): Int = {
    val index = edges.indexWhere(_.interface == interfaceRef)
    vassert(index >= 0)
    index
  }
  def getSInfoPtrElementIndex(): Int = {
    edges.size + 1
  }

  def getMemberLlvmIndex(memberIndex: Int): Int = {
    vassert(memberIndex < members.size)
    edges.size + 3 + memberIndex
  }

  def getTypeAndIndex(memberName: String): (StructFieldType4, Int) = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => vfail("wat " + this + " " + memberName)
      case Some((member, index)) => (member.fieldType, index)
    }
  }
}

case class StructMember4(name: String, fieldType: StructFieldType4)

trait StructFieldType4
case class VariableAddressStructFieldType4(referenceType: Reference4) extends StructFieldType4
case class ReferenceStructFieldType4(referenceType: Reference4) extends StructFieldType4


case class StructId4(midut: StructRef3) {
  def structId: Int = midut.structId;
}

case class Edge4(interface: InterfaceId4, structPrototypesByInterfacePrototype: ListMap[Prototype4, Prototype4])

case class ETable4(struct: StructId4, table: TetrisTable[InterfaceId4, InterfaceId4])



case class InterfaceDefinition4(
    hamut: InterfaceDefinition3,
    superInterfaces: List[InterfaceId4],
    prototypes: List[Prototype4]) {
  def interfaceId = hamut.interfaceId
  def mutability = hamut.mutability
  def humanName: String = hamut.humanName
  def getRef = InterfaceId4(hamut.getRef)
}

case class InterfaceId4(midut: InterfaceRef3) {
  def interfaceId: Int = midut.interfaceId
}

case class Program4(
    interfaces: List[InterfaceDefinition4],
    structs: List[StructDefinition4],
    externs: List[Prototype4],
    functions: List[Function4]) {
  def externFunctions = functions.filter(_.isExtern)
  def abstractFunctions = functions.filter(_.isAbstract)
  // Functions that are neither extern nor abstract
  def implementedFunctions = functions.filter(f => !f.isExtern && !f.isAbstract)
  // Abstract or implemented
  def nonExternFunctions = functions.filter(!_.isExtern)
}
