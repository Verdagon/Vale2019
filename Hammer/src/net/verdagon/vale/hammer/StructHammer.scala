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
        val (hamuts2, interfaceDefH) = translateInterfaceRef(hinputs, hamuts1, interfaceDef2.getRef)
        hamuts2
      }
    })
  }

  private def translateInterfaceRefs(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRefs2: List[InterfaceRef2]):
  (Hamuts, List[InterfaceRefH]) = {
    interfaceRefs2 match {
      case Nil => (hamuts0, Nil)
      case head2 :: tail2 => {
        val (hamuts1, headH) = translateInterfaceRef(hinputs, hamuts0, head2)
        val (hamuts2, tailH) = translateInterfaceRefs(hinputs, hamuts1, tail2)
        (hamuts2, headH :: tailH)
      }
    }
  }

  def translateInterfaceRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRef2: InterfaceRef2):
  (Hamuts, InterfaceRefH) = {
    hamuts0.interfaceRefs.get(interfaceRef2) match {
      case Some(structRefH) => (hamuts0, structRefH)
      case None => {
        val (hamuts1, fullNameH) = NameHammer.translateName(hinputs, hamuts0, interfaceRef2.fullName)
        // This is the only place besides InterfaceDefinitionH that can make a InterfaceRefH
        val temporaryInterfaceRefH = InterfaceRefH(hinputs.interfaceIds(interfaceRef2), fullNameH);
        val hamuts2 = hamuts1.forwardDeclareInterface(interfaceRef2, temporaryInterfaceRefH)
        val interfaceDef2 = hinputs.program2.lookupInterface(interfaceRef2);


        val edgeBlueprint = hinputs.edgeBlueprintsByInterface(interfaceRef2);

        val prototypes2 =
          edgeBlueprint.superFamilyRootBanners.map(superFamilyRootBanner => {
            hinputs.program2.lookupFunction(superFamilyRootBanner.toSignature).get.header.toPrototype
          })

        val (hamutsH, prototypesH) = FunctionHammer.translatePrototypes(hinputs, hamuts2, prototypes2)

        val interfaceDefH =
          InterfaceDefinitionH(
            temporaryInterfaceRefH.interfaceId,
            fullNameH,
            interfaceDef2.mutability,
            List() /* super interfaces */,
            prototypesH)
        val hamuts4 = hamutsH.addInterface(interfaceRef2, interfaceDefH)
        vassert(interfaceDefH.getRef == temporaryInterfaceRefH)
        (hamuts4, interfaceDefH.getRef)
      }
    }
  }

  def translateStructs(hinputs: Hinputs, hamuts0: Hamuts): Hamuts = {
    hinputs.program2.structs.foldLeft(hamuts0)({
      case (hamuts1, structDef2) => {
        val (hamuts2, structDefH) = translateStructRef(hinputs, hamuts1, structDef2.getRef)
        hamuts2
      }
    })
  }

  def translateStructRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef2: StructRef2):
  (Hamuts, StructRefH) = {
    hamuts0.structRefsByRef2.get(structRef2) match {
      case Some(structRefH) => (hamuts0, structRefH)
      case None => {
        val structId = hinputs.structIds(structRef2)
        val (hamuts1, fullNameH) = NameHammer.translateName(hinputs, hamuts0, structRef2.fullName)
        // This is the only place besides StructDefinitionH that can make a StructRefH
        val temporaryStructRefH = StructRefH(structId, fullNameH);
        val hamuts2 = hamuts1.forwardDeclareStruct(structRef2, temporaryStructRefH)
        val structDef2 = hinputs.program2.lookupStruct(structRef2);
        val (hamutsH, membersH) =
          TypeHammer.translateMembers(hinputs, hamuts2, structDef2.members)

        val (hamuts4, edgesH) = translateEdgesForStruct(hinputs, hamutsH, temporaryStructRefH, structRef2)

        val (hamuts5, eTableH) =
          hinputs.eTables.get(structRef2) match {
            case None => {
              (hamuts4, makeEmptyETable(hinputs, temporaryStructRefH))
            }
            case Some(eTable2) => {
              translateETable(hinputs, hamuts4, eTable2, temporaryStructRefH)
            }
          };

        val structDefH =
          StructDefinitionH(
            structId,
            fullNameH,
            structDef2.mutability,
            eTableH,
            edgesH,
            membersH);
        val hamuts6 = hamuts5.addStructOriginatingFromTemplar(structRef2, structDefH)
        vassert(structDefH.getRef == temporaryStructRefH)
        (hamuts6, structDefH.getRef)
      }
    }
  }

  def makeBox(hinputs: Hinputs, hamuts0a: Hamuts, conceptualVariability: Variability, type2: Coord, typeH: ReferenceH[ReferendH]):
  (Hamuts, StructRefH) = {
    val boxFullName2 = FullName2(List(NamePart2(BOX_HUMAN_NAME, Some(List(CoordTemplata(type2))))))
    val (hamuts0b, boxFullNameH) = NameHammer.translateName(hinputs, hamuts0a, boxFullName2)
    hamuts0b.structDefsById.find(_._2.fullName == boxFullNameH) match {
      case Some((_, structDefH)) => (hamuts0b, structDefH.getRef)
      case None => {
        val (hamuts1, structId) = hamuts0b.getNextStructId()
        vassert(!hamuts1.structDefsById.contains(structId))

        val temporaryStructRefH = StructRefH(structId, boxFullNameH);

        // We don't actually care about the given variability, because even if it's final, we still need
        // the box to contain a varying reference, see VCBAAF.
        val _ = conceptualVariability
        val actualVariability = Varying

        val memberH = StructMemberH(BOX_MEMBER_NAME, actualVariability, typeH)

        val structDefH =
          StructDefinitionH(
            structId,
            boxFullNameH,
            Mutable,
            ETableH(temporaryStructRefH, TetrisTable[InterfaceRefH, InterfaceRefH]((_ => 0), Array(), Array())),
            List(),
            List(memberH));
        val hamuts5 = hamuts1.addStructOriginatingFromHammer(structDefH)
        vassert(structDefH.getRef == temporaryStructRefH)
        (hamuts5, structDefH.getRef)
      }
    }
  }

  private def makeEmptyETable(
      hinputs: Hinputs,
      temporaryStructRefH: StructRefH):
  ETableH = {
    ETableH(
      temporaryStructRefH,
      TetrisTable[InterfaceRefH, InterfaceRefH](
        interfaceRefH => interfaceRefH.interfaceId,
        Array(None),
        Array()))
  }

  private def translateETable(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      eTable2: ETable2,
      temporaryStructRefH: StructRefH):
  (Hamuts, ETableH) = {

    val interfaceRefs2 =
      eTable2.table.combinedBuckets.toList.flatMap(
        _.toList.flatMap(entry => List(entry._1, entry._2)))

    val (hamuts1, interfaceRefsH) =
      StructHammer.translateInterfaceRefs(hinputs, hamuts0, interfaceRefs2)

    val interfaceRefs3ByInterfaceRef2 =
      interfaceRefs2.zip(interfaceRefsH).toMap

    val eTableH =
      ETableH(
        temporaryStructRefH,
        TetrisTable[InterfaceRefH, InterfaceRefH](
          interfaceRefH => interfaceRefH.interfaceId,
          eTable2.table.directory,
          eTable2.table.combinedBuckets.map({
            case None => None
            case Some((keyInterfaceRef2, valueInterfaceRef2)) => {
              vassert(keyInterfaceRef2 == valueInterfaceRef2)
              val interfaceRefH = interfaceRefs3ByInterfaceRef2(keyInterfaceRef2)
              Some((interfaceRefH, interfaceRefH))
            }
          })))

    (hamuts1, eTableH)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRefH: StructRefH,
      structRef2: StructRef2):
  (Hamuts, List[EdgeH]) = {
    val edges2 = hinputs.edges.filter(_.struct == structRef2)
    translateEdgesForStruct(hinputs, hamuts0, structRefH, edges2.toList)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRefH: StructRefH,
      edges2: List[Edge2]):
  (Hamuts, List[EdgeH]) = {
    edges2 match {
      case Nil => (hamuts0, Nil)
      case headEdge2 :: tailEdges2 => {
        val interfaceRef2 = headEdge2.interface
        val (hamuts1, interfaceRefH) = StructHammer.translateInterfaceRef(hinputs, hamuts0, interfaceRef2)
        val interfaceDefH = hamuts0.interfaceDefs(interfaceRef2)
        val (hamuts2, headEdgeH) = translateEdge(hinputs, hamuts1, structRefH, interfaceDefH, headEdge2)
        val (hamutsH, tailEdgesH) = translateEdgesForStruct(hinputs, hamuts2, structRefH, tailEdges2)
        (hamutsH, headEdgeH :: tailEdgesH)
      }
    }
  }


  private def translateEdge(hinputs: Hinputs, hamuts0: Hamuts, structRefH: StructRefH, interfaceDefH: InterfaceDefinitionH, edge2: Edge2):
  (Hamuts, EdgeH) = {
    val interfacePrototypesH = interfaceDefH.prototypes;
    val (hamuts1, prototypesH) = FunctionHammer.translatePrototypes(hinputs, hamuts0, edge2.methods)
    val structPrototypesByInterfacePrototype = ListMap[PrototypeH, PrototypeH](interfacePrototypesH.zip(prototypesH) : _*)
    (hamuts1, EdgeH(structRefH, interfaceDefH.getRef, structPrototypesByInterfacePrototype))
  }
}
