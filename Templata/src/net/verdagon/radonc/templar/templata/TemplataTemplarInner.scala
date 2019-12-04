package net.verdagon.radonc.templar.templata

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser.ShareP
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassert, vfail, vimpl}

import scala.collection.immutable.List

// Order of these members matters for comparison
case class TypeDistance(upcastDistance: Int, ownershipDistance: Int) {
  def lessThanOrEqualTo(that: TypeDistance): Boolean = {
    if (this.upcastDistance < that.upcastDistance) return true;
    if (this.upcastDistance > that.upcastDistance) return false;
    if (this.ownershipDistance < that.ownershipDistance) return true;
    if (this.ownershipDistance > that.ownershipDistance) return false;
    true
  }
}

trait ITemplataTemplarInnerDelegate[Env, State] {
  def lookupTemplata(env: Env, name: String): ITemplata

  def getMutability(state: State, kind: Kind): Mutability

  def getPackKind(env: Env, temputs0: State, types2: List[Coord]): (State, PackT2, Mutability)

  def evaluateStructTemplata(
    state0: State,
    templata: StructTemplata,
    templateArgs: List[ITemplata]):
  (State, Kind)

  def evaluateInterfaceTemplata(
    state0: State,
    templata: InterfaceTemplata,
    templateArgs: List[ITemplata]):
  (State, Kind)

  def getAncestorInterfaceDistance(
    temputs0: State,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (State, Option[Int])

  def getArraySequenceKind(
    env: Env,
    state0: State,
    mutability: Mutability,
    size: Int,
    element: Coord):
  (State, ArraySequenceT2)

  def getInterfaceTemplataType(it: InterfaceTemplata): ITemplataType
  def getStructTemplataType(st: StructTemplata): ITemplataType
}

class TemplataTemplarInner[Env, State](delegate: ITemplataTemplarInnerDelegate[Env, State]) {

//  def coerceTemplataToKind(
//    env: Env,
//    state0: State,
//    templata: ITemplata,
//    ownershipIfMutable: Ownership):
//  (State, Kind) = {
//    templata match {
//      case CoordTemplata(Coord(_, kind)) => (state0, kind)
//      case KindTemplata(kind) => (state0, kind)
//      case st @ StructTemplata(_, _) => delegate.evaluateStructTemplata(state0, st, List())
//      case it @ InterfaceTemplata(_, _) => delegate.evaluateInterfaceTemplata(state0, it, List())
//      case _ => vfail("not yet")
//    }
//  }
//  def coerceTemplataToReference(
//    env: Env,
//    state0: State,
//    templata: ITemplata,
//    ownershipIfMutable: Ownership):
//  (State, Coord) = {
//    templata match {
//      case CoordTemplata(reference) => (state0, reference)
//      case KindTemplata(referend) => {
//        (state0, pointifyReferend(state0, referend, ownershipIfMutable))
//      }
//      case st @ StructTemplata(_, _) => {
//        val (state2, kind) = delegate.evaluateStructTemplata(state0, st, List())
//        (state2, pointifyReferend(state2, kind, ownershipIfMutable))
//      }
//      case it @ InterfaceTemplata(_, _) => {
//        val (state2, kind) = delegate.evaluateInterfaceTemplata(state0, it, List())
//        (state2, pointifyReferend(state2, kind, ownershipIfMutable))
//      }
//      case _ => vfail("not yet")
//    }
//  }

  def evaluateTemplex(
    env: Env,
    state0: State,
    type1: ITemplexA):
  (State, ITemplata) = {
    vassert(type1.isInstanceOf[ITemplexA])
    type1 match {
      case NameAT(name, tyype) => {
        val thing = delegate.lookupTemplata(env, name)
        coerce(state0, thing, tyype)
      }
      case RepeaterSequenceAT(mutabilityTemplexS, sizeTemplexS, elementTemplexS, tyype) => {

        val (state1, MutabilityTemplata(mutability)) = evaluateTemplex(env, state0, mutabilityTemplexS)

        val (state2, IntegerTemplata(size)) = evaluateTemplex(env, state1, sizeTemplexS)

        val (state3, CoordTemplata(elementType2)) = evaluateTemplex(env, state2, elementTemplexS)

        val kind = KindTemplata(ArraySequenceT2(size, RawArrayT2(elementType2, mutability)))
        coerce(state3, kind, tyype)
      }
      case OwnershippedAT(ownershipS, innerType1) => {
        val ownership = Conversions.evaluateOwnership(ownershipS)
        val (state1, KindTemplata(innerKind)) = evaluateTemplex(env, state0, innerType1)
        val mutability = delegate.getMutability(state1, innerKind)
        vassert((mutability == Immutable) == (ownership == Share))
        (state1, CoordTemplata(Coord(ownership, innerKind)))
      }
      case NullableAT(_) => {
        //        val (state1, innerValueType2) = evaluateTemplex(env, state0, innerType1)
        //        val innerPointerType2 = TypeTemplar.pointify(innerValueType2)
        //        env.lookupType("Option") match {
        //          case TemplataStructTemplate(_) => {
        //            StructTemplar.getStructRef(env.globalEnv, state1, "Option", List(TemplataType(innerPointerType2)))
        //          }
        //        }
        vfail("support unions kkthx")
      }
      case CallAT(templateTemplexS, templateArgTemplexesS, resultType) => {
        val (state1, templateTemplata) = evaluateTemplex(env, state0, templateTemplexS)
        val (state2, templateArgsTemplatas) = evaluateTemplexes(env, state1, templateArgTemplexesS)
        templateTemplata match {
          case st @ StructTemplata(_, _) => {
            val (state3, kind) = delegate.evaluateStructTemplata(state2, st, templateArgsTemplatas)
            coerce(state3, KindTemplata(kind), resultType)
          }
          case it @ InterfaceTemplata(_, _) => {
            val (state3, kind) = delegate.evaluateInterfaceTemplata(state2, it, templateArgsTemplatas)
            coerce(state3, KindTemplata(kind), resultType)
          }
          case ArrayTemplateTemplata() => {
            val List(MutabilityTemplata(mutability), CoordTemplata(elementCoord)) = templateArgsTemplatas
            val result = UnknownSizeArrayT2(RawArrayT2(elementCoord, mutability))
            coerce(state2, KindTemplata(result), resultType)
          }
        }
      }
      case PackAT(memberTypeTemplexesS, resultType) => {
        val (state1, memberTemplatas) = evaluateTemplexes(env, state0, memberTypeTemplexesS)
        vassert(memberTemplatas.forall(_.tyype == CoordTemplataType))
        val memberCoords = memberTemplatas.map({ case CoordTemplata(c) => c })
        val (state2, packKind, _) = delegate.getPackKind(env, state1, memberCoords)
        coerce(state2, KindTemplata(packKind), resultType)
      }
      case x => {
        println(x)
        vfail("not yet " + x)
      }
    }
  }

  def evaluateTemplexes(
    env: Env,
    state0: State,
    types1: List[ITemplexA]):
  (State, List[ITemplata]) = {
    types1 match {
      case Nil => (state0, Nil)
      case head1 :: tail1 => {
        val (state1, head2) = evaluateTemplex(env, state0, head1);
        val (state2, tail2) = evaluateTemplexes(env, state1, tail1);
        (state2, head2 :: tail2)
      }
    }
  }

  def isTypeConvertible(
    temputs0: State,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (State, Boolean) = {
    val (temputs1, maybeDistance) =
      getTypeDistance(temputs0, sourcePointerType, targetPointerType)
    (temputs1, maybeDistance.nonEmpty)
  }

  def getTypeDistance(
    temputs0: State,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (State, Option[TypeDistance]) = {
    val Coord(targetOwnership, targetType) = targetPointerType;
    val Coord(sourceOwnership, sourceType) = sourcePointerType;

    if (sourceType == Never2()) {
      return (temputs0, Some(TypeDistance(0, 0)))
    }

    val (temputs2, upcastDistance) =
      if (sourceType == targetType) {
        (temputs0, 0)
      } else {
        (sourceType, targetType) match {
          case (Void2(), _) => return (temputs0, None)
          case (Int2(), _) => return (temputs0, None)
          case (Bool2(), _) => return (temputs0, None)
          case (Str2(), _) => return (temputs0, None)
          case (_, Void2()) => return (temputs0, None)
          case (_, Int2()) => return (temputs0, None)
          case (_, Bool2()) => return (temputs0, None)
          case (_, Str2()) => return (temputs0, None)
          case (_, StructRef2(_)) => return (temputs0, None)
          case (a @ StructRef2(_), b @ InterfaceRef2(_)) => {
            delegate.getAncestorInterfaceDistance(temputs0, a, b) match {
              case (temputs1, None) => return (temputs1, None)
              case (temputs1, Some(distance)) => (temputs1, distance)
            }
          }
          case (a @ InterfaceRef2(_), b @ InterfaceRef2(_)) => {
            delegate.getAncestorInterfaceDistance(temputs0, a, b) match {
              case (temputs1, None) => return (temputs1, None)
              case (temputs1, Some(distance)) => (temputs1, distance)
            }
          }
          case (PackT2(List(), _), Void2()) => vfail("figure out void<->emptypack")
          case (Void2(), PackT2(List(), _)) => vfail("figure out void<->emptypack")
          case (PackT2(List(), _), _) => return (temputs0, None)
          case (_, PackT2(List(), _)) => return (temputs0, None)
          case (_ : CitizenRef2, Int2() | Bool2() | Str2() | Float2()) => return (temputs0, None)
          case (Int2() | Bool2() | Str2() | Float2(), _ : CitizenRef2) => return (temputs0, None)
          case _ => {
            vfail("Can't convert from " + sourceType + " to " + targetType)
          }
        }
      }

    val ownershipDistance =
      (sourceOwnership, targetOwnership) match {
        case (Own, Own) => 0
        case (Borrow, Own) => return (temputs0, None)
        case (Own, Borrow) => 1
        case (Borrow, Borrow) => 0
        case (Raw, Raw) => 0
        case (Share, Share) => 0
      }

    (temputs2, Some(TypeDistance(upcastDistance, ownershipDistance)))
  }

  def isTypeTriviallyConvertible(
    temputs0: State,
    sourcePointerType: Coord,
    targetPointerType: Coord):
  (State, Boolean) = {
    val Coord(targetOwnership, targetType) = targetPointerType;
    val Coord(sourceOwnership, sourceType) = sourcePointerType;

    if (sourceType == Never2()) {
      return (temputs0, true)
    }

    val temputs2 =
      if (sourceType == targetType) {
        temputs0
      } else {
        (sourceType, targetType) match {
          case (Void2(), _) => return (temputs0, false)
          case (Int2(), _) => return (temputs0, false)
          case (Bool2(), _) => return (temputs0, false)
          case (Str2(), _) => return (temputs0, false)
          case (_, Void2()) => return (temputs0, false)
          case (_, Int2()) => return (temputs0, false)
          case (_, Bool2()) => return (temputs0, false)
          case (_, Str2()) => return (temputs0, false)
          case (_, StructRef2(_)) => return (temputs0, false)
          case (a @ StructRef2(_), b @ InterfaceRef2(_)) => {
            delegate.getAncestorInterfaceDistance(temputs0, a, b) match {
              case (temputs1, None) => return (temputs1, false)
              case (temputs1, Some(_)) => temputs1
            }
          }
          case (a @ InterfaceRef2(_), b @ InterfaceRef2(_)) => {
            delegate.getAncestorInterfaceDistance(temputs0, a, b) match {
              case (temputs1, None) => return (temputs1, false)
              case (temputs1, Some(_)) => temputs1
            }
          }
          case (PackT2(List(), _), Void2()) => vfail("figure out void<->emptypack")
          case (Void2(), PackT2(List(), _)) => vfail("figure out void<->emptypack")
          case (PackT2(List(), _), _) => return (temputs0, false)
          case (_, PackT2(List(), _)) => return (temputs0, false)
          case (_ : CitizenRef2, Int2() | Bool2() | Str2() | Float2()) => return (temputs0, false)
          case (Int2() | Bool2() | Str2() | Float2(), _ : CitizenRef2) => return (temputs0, false)
          case _ => {
            vfail("Can't convert from " + sourceType + " to " + targetType)
          }
        }
      }

    (sourceOwnership, targetOwnership) match {
      case (Own, Own) =>
      case (Borrow, Own) => return (temputs0, false)
      case (Own, Borrow) => return (temputs0, false)
      case (Borrow, Borrow) =>
      case (Raw, Raw) =>
      case (Share, Share) =>
    }

    (temputs2, true)
  }

  def pointifyReferend(state: State, referend: Kind, ownershipIfMutable: Ownership): Coord = {
    referend match {
      case a @ UnknownSizeArrayT2(array) => {
        val ownership = if (array.mutability == Mutable) ownershipIfMutable else Share
        Coord(ownership, a)
      }
      case a @ ArraySequenceT2(_, RawArrayT2(_, mutability)) => {
        val ownership = if (mutability == Mutable) ownershipIfMutable else Share
        Coord(ownership, a)
      }
      case a @ PackT2(_, underlyingStruct) => {
        val ownership = if (delegate.getMutability(state, underlyingStruct) == Mutable) ownershipIfMutable else Share
        Coord(ownership, a)
      }
      case s @ StructRef2(_) => {
        val ownership = if (delegate.getMutability(state, s) == Mutable) ownershipIfMutable else Share
        Coord(ownership, s)
      }
      case i @ InterfaceRef2(_) => {
        val ownership = if (delegate.getMutability(state, i) == Mutable) ownershipIfMutable else Share
        Coord(ownership, i)
      }
      case Void2() => {
        Coord(Raw, Void2())
      }
      case Int2() => {
        Coord(Share, Int2())
      }
      case Float2() => {
        Coord(Share, Float2())
      }
      case Bool2() => {
        Coord(Share, Bool2())
      }
      case Str2() => {
        Coord(Share, Str2())
      }
    }
  }

  def pointifyReferends(state: State, valueTypes: List[Kind], ownershipIfMutable: Ownership): List[Coord] = {
    valueTypes.map(valueType => pointifyReferend(state, valueType, ownershipIfMutable))
  }


  def evaluateStructTemplata(
    state0: State,
    template: StructTemplata,
    templateArgs: List[ITemplata],
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val (state1, uncoercedTemplata) =
      delegate.evaluateStructTemplata(state0, template, templateArgs)
    val (state2, templata) =
      coerce(state1, KindTemplata(uncoercedTemplata), expectedType)
    (state2, templata)
  }

  def evaluateInterfaceTemplata(
    state0: State,
    template: InterfaceTemplata,
    templateArgs: List[ITemplata],
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val (state1, uncoercedTemplata) =
      delegate.evaluateInterfaceTemplata(state0, template, templateArgs)
    val (state2, templata) =
      coerce(state1, KindTemplata(uncoercedTemplata), expectedType)
    (state2, templata)
  }

  def evaluateBuiltinTemplateTemplata(
    state0: State,
    template: ArrayTemplateTemplata,
    templateArgs: List[ITemplata],
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val List(MutabilityTemplata(mutability), CoordTemplata(elementType)) = templateArgs
    val arrayKindTemplata = KindTemplata(UnknownSizeArrayT2(RawArrayT2(elementType, mutability)))
    val (state2, templata) =
      coerce(state0, arrayKindTemplata, expectedType)
    (state2, templata)
  }

  def getPackKind(
    env: Env,
    state0: State,
    members: List[Coord],
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val (state1, uncoercedTemplata, _) =
      delegate.getPackKind(env, state0, members)
    val (state2, templata) =
      coerce(state1, KindTemplata(uncoercedTemplata), expectedType)
    (state2, templata)
  }

  def getArraySequenceKind(
    env: Env,
    state0: State,
    mutability: Mutability,
    size: Int,
    element: Coord,
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val (state1, uncoercedTemplata) =
      delegate.getArraySequenceKind(env, state0, mutability, size, element)
    val (state2, templata) =
      coerce(state1, KindTemplata(uncoercedTemplata), expectedType)
    (state2, templata)
  }

  def lookupTemplata(
    env: Env,
    state: State,
    name: String,
    expectedType: ITemplataType):
  (State, ITemplata) = {
    val uncoercedTemplata = delegate.lookupTemplata(env, name)
    coerce(state, uncoercedTemplata, expectedType)
  }

  def coerce(
    state0: State,
    templata: ITemplata,
    tyype: ITemplataType):
  (State, ITemplata) = {
    println("clean up that raw/never thing!")
    // debt: find a way to simplify this function, seems simplifiable
    (templata, tyype) match {
      case (MutabilityTemplata(_), MutabilityTemplataType) => {
        (state0, templata)
      }
      case (KindTemplata(kind), CoordTemplataType) => {
        val mutability = delegate.getMutability(state0, kind)
        val coerced =
          CoordTemplata(
            Coord(
              if (kind == Void2() || kind == Never2()) Raw else if (mutability == Mutable) Own else Share,
              kind))
        (state0, coerced)
      }
      case (KindTemplata(_), KindTemplataType) => (state0, templata)
      case (CoordTemplata(_), CoordTemplataType) => (state0, templata)
      case (st @ StructTemplata(_, structA), KindTemplataType) => {
        if (structA.isTemplate) {
          vfail("Can't coerce " + structA.name + " to be a kind, is a template!")
        }
        val (state1, kind) =
          delegate.evaluateStructTemplata(state0, st, List())
        (state1, KindTemplata(kind))
      }
      case (it @ InterfaceTemplata(_, interfaceA), KindTemplataType) => {
        if (interfaceA.isTemplate) {
          vfail("Can't coerce " + interfaceA.name + " to be a kind, is a template!")
        }
        val (state1, kind) =
          delegate.evaluateInterfaceTemplata(state0, it, List())
        (state1, KindTemplata(kind))
      }
      case (st @ StructTemplata(_, structA), ttt @ TemplateTemplataType(_, _)) => {
        vassert(structA.isTemplate)
        vassert(delegate.getStructTemplataType(st) == ttt)
        (state0, st)
      }
      case (st @ StructTemplata(_, structA), CoordTemplataType) => {
        if (structA.isTemplate) {
          vfail("Can't coerce " + structA.name + " to be a coord, is a template!")
        }
        val (state1, kind) =
          delegate.evaluateStructTemplata(state0, st, List())
        val mutability = delegate.getMutability(state1, kind)
        val coerced =
          CoordTemplata(Coord(if (mutability == Mutable) Own else Share, kind))
        (state1, coerced)
      }
      case (it @ InterfaceTemplata(_, interfaceA), CoordTemplataType) => {
        if (interfaceA.isTemplate) {
          vfail("Can't coerce " + interfaceA.name + " to be a coord, is a template!")
        }
        val (state1, kind) =
          delegate.evaluateInterfaceTemplata(state0, it, List())
        val mutability = delegate.getMutability(state1, kind)
        val coerced =
          CoordTemplata(Coord(if (mutability == Mutable) Own else Share, kind))
        (state1, coerced)
      }
      case (it @ InterfaceTemplata(_, interfaceA), ttt @ TemplateTemplataType(_, _)) => {
        vassert(interfaceA.isTemplate)
        vassert(delegate.getInterfaceTemplataType(it) == ttt)
        (state0, it)
      }
      case (btt @ ArrayTemplateTemplata(), ttt @ TemplateTemplataType(_, _)) => {
        vassert(btt.tyype == ttt)
        (state0, btt)
      }
      case _ => vimpl((templata, tyype).toString())
    }
  }
}
