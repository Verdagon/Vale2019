package net.verdagon.vale.hammer

import net.verdagon.vale.templar.templata.Prototype2
import net.verdagon.vale.templar.types.{InterfaceRef2, PackT2, StructRef2}
import net.verdagon.vale.vassert


case class Hamuts(
    structRefsByRef2: Map[StructRef2, StructRef3],
    structDefsByRef2: Map[StructRef2, StructDefinition3],
    structDefsById: Map[Int, StructDefinition3],
    nextStructId: Int,
    interfaceRefs: Map[InterfaceRef2, InterfaceRef3],
    interfaceDefs: Map[InterfaceRef2, InterfaceDefinition3],
    functionRefs: Map[Prototype2, FunctionRef3],
    functionDefs: Map[Prototype2, Function3]) {
  private def addPackStruct(pack: PackT2, structDef: StructDefinition3): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareStruct(structRef2: StructRef2, structRef3: StructRef3): Hamuts = {
    Hamuts(
      structRefsByRef2 + (structRef2 -> structRef3),
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromTemplar(structRef2: StructRef2, structDef3: StructDefinition3): Hamuts = {
    vassert(structRefsByRef2.contains(structRef2))
    vassert(!structDefsById.contains(structDef3.structId))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2 + (structRef2 -> structDef3),
      structDefsById + (structDef3.structId -> structDef3),
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromHammer(structDef3: StructDefinition3): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById + (structDef3.structId -> structDef3),
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareInterface(interfaceRef2: InterfaceRef2, interfaceRef3: InterfaceRef3): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs + (interfaceRef2 -> interfaceRef3),
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addInterface(interfaceRef2: InterfaceRef2, interfaceDef3: InterfaceDefinition3): Hamuts = {
    vassert(interfaceRefs.contains(interfaceRef2))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs + (interfaceRef2 -> interfaceDef3),
      functionRefs,
      functionDefs)
  }

  def forwardDeclareFunction(functionRef2: Prototype2, functionRef3: FunctionRef3): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs + (functionRef2 -> functionRef3),
      functionDefs)
  }

  def addFunction(functionRef2: Prototype2, functionDef3: Function3): Hamuts = {
    vassert(functionRefs.contains(functionRef2))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs + (functionRef2 -> functionDef3))
  }

  def getNextStructId(): (Hamuts, Int) = {
    (Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId + 1,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs),
        nextStructId)
  }
}
