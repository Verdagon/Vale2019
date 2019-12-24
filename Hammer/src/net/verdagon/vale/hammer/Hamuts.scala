package net.verdagon.vale.hammer

import net.verdagon.vale.templar.templata.Prototype2
import net.verdagon.vale.templar.types.{InterfaceRef2, PackT2, StructRef2}
import net.verdagon.vale.vassert


case class Hamuts(
    structRefsByRef2: Map[StructRef2, StructRefH],
    structDefsByRef2: Map[StructRef2, StructDefinitionH],
    structDefsById: Map[Int, StructDefinitionH],
    nextStructId: Int,
    interfaceRefs: Map[InterfaceRef2, InterfaceRefH],
    interfaceDefs: Map[InterfaceRef2, InterfaceDefinitionH],
    functionRefs: Map[Prototype2, FunctionRefH],
    functionDefs: Map[Prototype2, FunctionH]) {
  private def addPackStruct(pack: PackT2, structDef: StructDefinitionH): Hamuts = {
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

  def forwardDeclareStruct(structRef2: StructRef2, structRefH: StructRefH): Hamuts = {
    Hamuts(
      structRefsByRef2 + (structRef2 -> structRefH),
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromTemplar(structRef2: StructRef2, structDefH: StructDefinitionH): Hamuts = {
    vassert(structRefsByRef2.contains(structRef2))
    vassert(!structDefsById.contains(structDefH.structId))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2 + (structRef2 -> structDefH),
      structDefsById + (structDefH.structId -> structDefH),
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStructOriginatingFromHammer(structDefH: StructDefinitionH): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById + (structDefH.structId -> structDefH),
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareInterface(interfaceRef2: InterfaceRef2, interfaceRefH: InterfaceRefH): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs + (interfaceRef2 -> interfaceRefH),
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addInterface(interfaceRef2: InterfaceRef2, interfaceDefH: InterfaceDefinitionH): Hamuts = {
    vassert(interfaceRefs.contains(interfaceRef2))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs + (interfaceRef2 -> interfaceDefH),
      functionRefs,
      functionDefs)
  }

  def forwardDeclareFunction(functionRef2: Prototype2, functionRefH: FunctionRefH): Hamuts = {
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs + (functionRef2 -> functionRefH),
      functionDefs)
  }

  def addFunction(functionRef2: Prototype2, functionDefH: FunctionH): Hamuts = {
    vassert(functionRefs.contains(functionRef2))
    Hamuts(
      structRefsByRef2,
      structDefsByRef2,
      structDefsById,
      nextStructId,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs + (functionRef2 -> functionDefH))
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
