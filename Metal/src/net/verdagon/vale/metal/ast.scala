package net.verdagon.vale.metal

import net.verdagon.vale.{vassert, vcurious, vfail, vimpl}
import net.verdagon.von.{IVonData, VonArray, VonStr}

import scala.collection.immutable.ListMap

case class ProgramH(
  interfaces: List[InterfaceDefinitionH],
  structs: List[StructDefinitionH],
  // TODO: Get rid of this; since there's no IDs anymore we can have a stable
  // hardcoded FullNameH("__Pack", Some(List()), None, None).
  emptyPackStructRef: StructRefH,
  externs: List[PrototypeH],
  functions: List[FunctionH]) {
  def externFunctions = functions.filter(_.isExtern)
  def abstractFunctions = functions.filter(_.isAbstract)
  // Functions that are neither extern nor abstract
  def getAllUserImplementedFunctions = functions.filter(f => f.isUserFunction && !f.isExtern && !f.isAbstract)
  // Abstract or implemented
  def nonExternFunctions = functions.filter(!_.isExtern)
  def getAllUserFunctions = functions.filter(_.isUserFunction)
  def main() = {
    val matching = functions.filter(_.fullName.von.members.last == VonStr("main"))
    vassert(matching.size == 1)
    matching.head
  }

  def lookupFunction(humanName: String) = {
    val matches = functions.filter(_.fullName.von.members.last == vimpl(humanName))
    vassert(matches.size == 1)
    matches.head
  }
}

case class StructDefinitionH(
    fullName: FullNameH,
    mutability: Mutability,
    edges: List[EdgeH],
    members: List[StructMemberH]) {

  def getRef: StructRefH = StructRefH(fullName)

  // These functions are tightly coupled with StructSculptor.declareStructInfo
  def getInterfacePtrElementIndex(interfaceRef: InterfaceRefH): Int = {
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

  def getTypeAndIndex(memberName: String): (ReferenceH[ReferendH], Int) = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => vfail("wat " + this + " " + memberName)
      case Some((member, index)) => (member.tyype, index)
    }
  }
}

case class StructMemberH(
  name: FullNameH,
  variability: Variability,
  tyype: ReferenceH[ReferendH])

case class InterfaceDefinitionH(
  fullName: FullNameH,
  mutability: Mutability,
  // TODO: Change this to edges, since interfaces impl other interfaces.
  superInterfaces: List[InterfaceRefH],
  prototypes: List[PrototypeH]) {
  def getRef = InterfaceRefH(fullName)
}

// Represents how a struct implements an interface.
// Each edge has a vtable.
case class EdgeH(
  struct: StructRefH,
  interface: InterfaceRefH,
  structPrototypesByInterfacePrototype: ListMap[PrototypeH, PrototypeH])

case class FunctionH(
  prototype: PrototypeH,
  // TODO: Get rid of this, since it's only for testing. Perhaps use an external set?
  isAbstract: Boolean,
  // TODO: Get rid of this, since it's only for testing. Perhaps use an external set?
  isExtern: Boolean,
  // TODO: Get rid of this, since it's only for testing. Perhaps use an external set?
  isUserFunction: Boolean,
  block: BlockH) {
  def fullName = prototype.fullName
}

case class PrototypeH(
  fullName: FullNameH,
  // TODO: Perhaps get rid of this, since we can get it from the full name.
  params: List[ReferenceH[ReferendH]],
  returnType: ReferenceH[ReferendH]
)

case class FullNameH(von: VonArray) {
  def addStep(s: String) = {
    FullNameH(VonArray(von.id, von.members :+ VonStr(s)))
  }
}

case class CodeLocationH(
  file: String,
  line: Int,
  char: Int)
