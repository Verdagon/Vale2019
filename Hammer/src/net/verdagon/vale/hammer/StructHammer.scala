package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.{ETable2, Hinputs, TetrisTable}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.CoordTemplata
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vassert

import scala.collection.immutable.ListMap

object StructHammer {
  val BOX_HUMAN_NAME = "__Box"
  val BOX_MEMBER_INDEX = 0
  val BOX_MEMBER_NAME = "__boxee"

  def translateInterfaces(hinputs: Hinputs, hamuts0: Hamuts): Hamuts = {
    hinputs.program2.interfaces.foldLeft(hamuts0)({
      case (hamuts1, interfaceDef2) => {
        val (hamuts2, interfaceDef3) = translateInterfaceRef(hinputs, hamuts1, interfaceDef2.getRef)
        hamuts2
      }
    })
  }

  private def translateInterfaceRefs(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRefs2: List[InterfaceRef2]):
  (Hamuts, List[InterfaceRef3]) = {
    interfaceRefs2 match {
      case Nil => (hamuts0, Nil)
      case head2 :: tail2 => {
        val (hamuts1, head3) = translateInterfaceRef(hinputs, hamuts0, head2)
        val (hamuts2, tail3) = translateInterfaceRefs(hinputs, hamuts1, tail2)
        (hamuts2, head3 :: tail3)
      }
    }
  }

  def translateInterfaceRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRef2: InterfaceRef2):
  (Hamuts, InterfaceRef3) = {
    hamuts0.interfaceRefs.get(interfaceRef2) match {
      case Some(structRef3) => (hamuts0, structRef3)
      case None => {
        val (hamuts1, fullName3) = NameHammer.translateName(hinputs, hamuts0, interfaceRef2.fullName)
        // This is the only place besides InterfaceDefinition3 that can make a InterfaceRef3
        val temporaryInterfaceRef3 = InterfaceRef3(hinputs.interfaceIds(interfaceRef2), fullName3);
        val hamuts2 = hamuts1.forwardDeclareInterface(interfaceRef2, temporaryInterfaceRef3)
        val interfaceDef2 = hinputs.program2.lookupInterface(interfaceRef2);


        val edgeBlueprint = hinputs.edgeBlueprintsByInterface(interfaceRef2);

        val prototypes2 =
          edgeBlueprint.superFamilyRootBanners.map(superFamilyRootBanner => {
            hinputs.program2.lookupFunction(superFamilyRootBanner.toSignature).get.header.toPrototype
          })

        val (hamuts3, prototypes3) = FunctionHammer.translatePrototypes(hinputs, hamuts2, prototypes2)

        val interfaceDef3 =
          InterfaceDefinition3(
            temporaryInterfaceRef3.interfaceId,
            fullName3,
            interfaceDef2.mutability,
            List() /* super interfaces */,
            prototypes3)
        val hamuts4 = hamuts3.addInterface(interfaceRef2, interfaceDef3)
        vassert(interfaceDef3.getRef == temporaryInterfaceRef3)
        (hamuts4, interfaceDef3.getRef)
      }
    }
  }

  def translateStructs(hinputs: Hinputs, hamuts0: Hamuts): Hamuts = {
    hinputs.program2.structs.foldLeft(hamuts0)({
      case (hamuts1, structDef2) => {
        val (hamuts2, structDef3) = translateStructRef(hinputs, hamuts1, structDef2.getRef)
        hamuts2
      }
    })
  }

  def translateStructRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef2: StructRef2):
  (Hamuts, StructRef3) = {
    hamuts0.structRefsByRef2.get(structRef2) match {
      case Some(structRef3) => (hamuts0, structRef3)
      case None => {
        val structId = hinputs.structIds(structRef2)
        val (hamuts1, fullName3) = NameHammer.translateName(hinputs, hamuts0, structRef2.fullName)
        // This is the only place besides StructDefinition3 that can make a StructRef3
        val temporaryStructRef3 = StructRef3(structId, fullName3);
        val hamuts2 = hamuts1.forwardDeclareStruct(structRef2, temporaryStructRef3)
        val structDef2 = hinputs.program2.lookupStruct(structRef2);
        val (hamuts3, members3) =
          TypeHammer.translateMembers(hinputs, hamuts2, structDef2.members)

        val (hamuts4, edges3) = translateEdgesForStruct(hinputs, hamuts3, temporaryStructRef3, structRef2)

        val (hamuts5, eTable3) =
          hinputs.eTables.get(structRef2) match {
            case None => {
              (hamuts4, makeEmptyETable(hinputs, temporaryStructRef3))
            }
            case Some(eTable2) => {
              translateETable(hinputs, hamuts4, eTable2, temporaryStructRef3)
            }
          };

        val structDef3 =
          StructDefinition3(
            structId,
            fullName3,
            structDef2.mutability,
            eTable3,
            edges3,
            members3);
        val hamuts6 = hamuts5.addStructOriginatingFromTemplar(structRef2, structDef3)
        vassert(structDef3.getRef == temporaryStructRef3)
        (hamuts6, structDef3.getRef)
      }
    }
  }

  def makeBox(hinputs: Hinputs, hamuts0a: Hamuts, conceptualVariability: Variability, type2: Coord, type3: Reference3[Referend3]):
  (Hamuts, StructRef3) = {
    val boxFullName2 = FullName2(List(NamePart2(BOX_HUMAN_NAME, Some(List(CoordTemplata(type2))))))
    val (hamuts0b, boxFullName3) = NameHammer.translateName(hinputs, hamuts0a, boxFullName2)
    hamuts0b.structDefsById.find(_._2.fullName == boxFullName3) match {
      case Some((_, structDef3)) => (hamuts0b, structDef3.getRef)
      case None => {
        val (hamuts1, structId) = hamuts0b.getNextStructId()
        vassert(!hamuts1.structDefsById.contains(structId))

        val temporaryStructRef3 = StructRef3(structId, boxFullName3);

        // We don't actually care about the given variability, because even if it's final, we still need
        // the box to contain a varying reference, see VCBAAF.
        val _ = conceptualVariability
        val actualVariability = Varying

        val member3 = StructMember3(BOX_MEMBER_NAME, actualVariability, type3)

        val structDef3 =
          StructDefinition3(
            structId,
            boxFullName3,
            Mutable,
            ETable3(temporaryStructRef3, TetrisTable[InterfaceRef3, InterfaceRef3]((_ => 0), Array(), Array())),
            List(),
            List(member3));
        val hamuts5 = hamuts1.addStructOriginatingFromHammer(structDef3)
        vassert(structDef3.getRef == temporaryStructRef3)
        (hamuts5, structDef3.getRef)
      }
    }
  }

  private def makeEmptyETable(
      hinputs: Hinputs,
      temporaryStructRef3: StructRef3):
  ETable3 = {
    ETable3(
      temporaryStructRef3,
      TetrisTable[InterfaceRef3, InterfaceRef3](
        interfaceRef3 => interfaceRef3.interfaceId,
        Array(None),
        Array()))
  }

  private def translateETable(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      eTable2: ETable2,
      temporaryStructRef3: StructRef3):
  (Hamuts, ETable3) = {

    val interfaceRefs2 =
      eTable2.table.combinedBuckets.toList.flatMap(
        _.toList.flatMap(entry => List(entry._1, entry._2)))

    val (hamuts1, interfaceRefs3) =
      StructHammer.translateInterfaceRefs(hinputs, hamuts0, interfaceRefs2)

    val interfaceRefs3ByInterfaceRef2 =
      interfaceRefs2.zip(interfaceRefs3).toMap

    val eTable3 =
      ETable3(
        temporaryStructRef3,
        TetrisTable[InterfaceRef3, InterfaceRef3](
          interfaceRef3 => interfaceRef3.interfaceId,
          eTable2.table.directory,
          eTable2.table.combinedBuckets.map({
            case None => None
            case Some((keyInterfaceRef2, valueInterfaceRef2)) => {
              vassert(keyInterfaceRef2 == valueInterfaceRef2)
              val interfaceRef3 = interfaceRefs3ByInterfaceRef2(keyInterfaceRef2)
              Some((interfaceRef3, interfaceRef3))
            }
          })))

    (hamuts1, eTable3)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef3: StructRef3,
      structRef2: StructRef2):
  (Hamuts, List[Edge3]) = {
    val edges2 = hinputs.edges.filter(_.struct == structRef2)
    translateEdgesForStruct(hinputs, hamuts0, structRef3, edges2.toList)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef3: StructRef3,
      edges2: List[Edge2]):
  (Hamuts, List[Edge3]) = {
    edges2 match {
      case Nil => (hamuts0, Nil)
      case headEdge2 :: tailEdges2 => {
        val interfaceRef2 = headEdge2.interface
        val (hamuts1, interfaceRef3) = StructHammer.translateInterfaceRef(hinputs, hamuts0, interfaceRef2)
        val interfaceDef3 = hamuts0.interfaceDefs(interfaceRef2)
        val (hamuts2, headEdge3) = translateEdge(hinputs, hamuts1, structRef3, interfaceDef3, headEdge2)
        val (hamuts3, tailEdges3) = translateEdgesForStruct(hinputs, hamuts2, structRef3, tailEdges2)
        (hamuts3, headEdge3 :: tailEdges3)
      }
    }
  }


  private def translateEdge(hinputs: Hinputs, hamuts0: Hamuts, structRef3: StructRef3, interfaceDef3: InterfaceDefinition3, edge2: Edge2):
  (Hamuts, Edge3) = {
    val interfacePrototypes3 = interfaceDef3.prototypes;
    val (hamuts1, prototypes3) = FunctionHammer.translatePrototypes(hinputs, hamuts0, edge2.methods)
    val structPrototypesByInterfacePrototype = ListMap[Prototype3, Prototype3](interfacePrototypes3.zip(prototypes3) : _*)
    (hamuts1, Edge3(structRef3, interfaceDef3.getRef, structPrototypesByInterfacePrototype))
  }
}
