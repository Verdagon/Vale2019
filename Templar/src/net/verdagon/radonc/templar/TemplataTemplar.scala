package net.verdagon.radonc.templar.templata

import net.verdagon.radonc.astronomer.{ITemplataType, ITemplexA, TemplateTemplataType}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.citizen.{ImplTemplar, StructTemplar}
import net.verdagon.radonc.templar.env.{IEnvironment, TemplataLookupContext}
import net.verdagon.radonc.{vassertSome, vfail}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._

import scala.collection.immutable.{List, Map, Set}

object TemplataTemplar {

  def getTypeDistance(
    temputs0: Temputs,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (Temputs, Option[TypeDistance]) = {
    makeInner().getTypeDistance(temputs0,sourcePointerType, targetPointerType)
  }

  def evaluateTemplex(
    env: IEnvironment,
    temputs: Temputs,
    templex: ITemplexA
  ): (Temputs, ITemplata) = {
    makeInner().evaluateTemplex(env, temputs, templex)
  }

  def evaluateTemplexes(env: IEnvironment, temputs0: Temputs, templexes: List[ITemplexA]):
  (Temputs, List[ITemplata]) = {
    makeInner().evaluateTemplexes(env, temputs0, templexes)
  }

  def pointifyReferend(
    temputs0: Temputs,
    referend: Kind,
    ownershipIfMutable: Ownership):
  Coord = {
    makeInner().pointifyReferend(temputs0, referend, ownershipIfMutable)
  }

  def isTypeConvertible(
    temputs0: Temputs,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (Temputs, Boolean) = {
    makeInner().isTypeConvertible(temputs0, sourcePointerType, targetPointerType)
  }

  def isTypeTriviallyConvertible(
    temputs0: Temputs,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (Temputs, Boolean) = {
    makeInner().isTypeTriviallyConvertible(temputs0, sourcePointerType, targetPointerType)
  }

//
//  def lookupTemplata(
//    env: IEnvironment,
//    temputs0: Temputs,
//    name: String
//  ): (Temputs, Option[ITemplata]) = {
//    env match {
//      case IEnvironment(globalEnv, _, _, _, templatas) => {
//        templatas.get(name) match {
//          case Some(templata) => (temputs0, Some(templata))
//          case None => {
//            lookupTemplataFromGlobalEnv(globalEnv, temputs0, name)
//          }
//        }
//      }
//      case globalEnv @ IEnvironment(_, _, _, _, _, _, _, _, _) => {
//        lookupTemplataFromGlobalEnv(globalEnv, temputs0, name)
//      }
//    }
//  }
//
//  private def lookupTemplataFromGlobalEnv(
//    env: IEnvironment,
//    temputs0: Temputs,
//    name: String
//  ): (Temputs, Option[ITemplata]) = {
//    val IEnvironment(
//      ordinaryStructBanners,
//      ordinaryInterfaceBanners,
//      templatedStructBanners,
//      templatedInterfaceBanners,
//      _,
//      _,
//      _,
//      _,
//      templatas) = env
//
//    templatas.get(name) match {
//      case Some(templata) => return (temputs0, Some(templata))
//      case None =>
//    }
//    ordinaryStructBanners.get(name) match {
//      case Some(osb) => {
//        val (temputs1, structRef) =
//          StructTemplar.getStructRef(env.globalEnv, temputs0, osb)
//        return (temputs1, Some(ReferendTemplata(structRef)))
//      }
//      case None =>
//    }
//    ordinaryInterfaceBanners.get(name) match {
//      case Some(oib) => {
//        val (temputs1, structRef) =
//          StructTemplar.getInterfaceRef(env.globalEnv, temputs0, oib)
//        return (temputs1, Some(ReferendTemplata(structRef)))
//      }
//      case None =>
//    }
//    templatedStructBanners.get(name) match {
//      case Some(osb) => {
//        return (temputs0, Some(StructTerryTemplata(StructTerry(None, name, List()))))
//      }
//      case None =>
//    }
//    templatedInterfaceBanners.get(name) match {
//      case Some(osb) => {
//        return (temputs0, Some(InterfaceTerryTemplata(InterfaceTerry(None, name, List()))))
//      }
//      case None =>
//    }
//    return (temputs0, None)
//  }

  def makeInner(): TemplataTemplarInner[IEnvironment, Temputs] = {
    new TemplataTemplarInner[IEnvironment, Temputs](
      new ITemplataTemplarInnerDelegate[IEnvironment, Temputs] {
        override def lookupTemplata(env: IEnvironment, name: String): ITemplata = {
          // Changed this from AnythingLookupContext to TemplataLookupContext
          // because this is called from StructTemplar to figure out its members.
          // We could instead pipe a lookup context through, if this proves problematic.
          vassertSome(env.getNearestTemplataWithName(name, Set(TemplataLookupContext)))
        }

        override def getMutability(temputs: Temputs, kind: Kind): Mutability = {
          Templar.getMutability(temputs, kind)
        }

        override def getPackKind(env: IEnvironment, temputs0: Temputs, types2: List[Coord]):
        (Temputs, PackT2, Mutability) = {
          PackTemplar.makePackType(env.globalEnv, temputs0, types2)
        }

        override def evaluateInterfaceTemplata(state0: Temputs, templata: InterfaceTemplata, templateArgs: List[ITemplata]):
        (Temputs, Kind) = {
          StructTemplar.getInterfaceRef(state0, templata, templateArgs)
        }

        override def evaluateStructTemplata(state0: Temputs, templata: StructTemplata, templateArgs: List[ITemplata]):
        (Temputs, Kind) = {
          StructTemplar.getStructRef(state0, templata, templateArgs)
        }

        //val elementCoord =
        //  templateArgTemplatas match {
        //    case List(ReferenceTemplata(ref)) => ref
        //    // todo: coerce referend into reference here... or somehow reuse all the machinery we have for
        //    // regular templates?
        //  }
        //val elementMutability = Templar.getMutability(state0, elementCoord.referend)
        //if (arrayMutability == Immutable && elementMutability == Mutable) {
        //  throw new Exception("Can't have an immutable array of mutables")
        //}
        //val arrayType = UnknownSizeArrayT2(RawArrayT2(elementCoord, arrayMutability))
        //(state0, ReferendTemplata(arrayType))

        override def getAncestorInterfaceDistance(
          temputs0: Temputs,
          descendantCitizenRef: CitizenRef2,
          ancestorInterfaceRef: InterfaceRef2):
        (Temputs, Option[Int]) = {
          ImplTemplar.getAncestorInterfaceDistance(temputs0, descendantCitizenRef, ancestorInterfaceRef)
        }

        override def getArraySequenceKind(env: IEnvironment, state0: Temputs, mutability: Mutability, size: Int, element: Coord): (Temputs, ArraySequenceT2) = {
          ArrayTemplar.makeArraySequenceType(env, state0, mutability, size, element)
        }

        override def getInterfaceTemplataType(it: InterfaceTemplata): ITemplataType = {
          it.originInterface.tyype
        }

        override def getStructTemplataType(st: StructTemplata): ITemplataType = {
          st.originStruct.tyype
        }
      })
  }

}
