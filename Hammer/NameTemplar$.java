package net.verdagon.vale.templar;

object NameTemplar {
  // Identifier names need to come from the Templar output because some things are erased
  // by Hammer types, such as template args. Hammer will sometimes output many functions
  // with the same signature because of this.

  def getReferenceIdentifierName(reference: Coord): String = {
    val Coord(ownership, referend) = reference;
    val ownershipString =
      ownership match {
        case Share => "*"
        case Borrow => "&"
        case Own => "^"
        case Raw => "%"
      }
    ownershipString + getReferendIdentifierName(referend)
  }

  def getFullNameIdentifierName(fullName: FullName2): String = {
    fullName.steps.map({
      case NamePart2(humanName, None) => humanName
      case NamePart2(humanName, Some(templateArgs)) => {
        humanName + "<" + templateArgs.map(getIdentifierName).mkString(", ") + ">"
      }
    }).mkString("::")
  }

  def getReferendIdentifierName(tyype: Kind): String = {
    tyype match {
      case Int2() => "𝒾"
      case Float2() => "𝒻"
      case Bool2() => "𝒷"
      case Str2() => "𝓈"
      case Void2() => "∅"
      case UnknownSizeArrayT2(array) => "𝔸" + getReferenceIdentifierName(array.memberType)
      case ArraySequenceT2(size, arrayT2) => "𝔸" + size + getReferenceIdentifierName(arrayT2.memberType)
      case FunctionT2(paramTypes, returnType) => {
        "𝔽(" + paramTypes.map(getReferenceIdentifierName).mkString(",") + "):" + getReferenceIdentifierName(returnType)
      }
      case PackT2(innerTypes, underlyingStruct) => {
        getReferendIdentifierName(underlyingStruct)
      }
      case StructRef2(fullName) => "𝕊" + getFullNameIdentifierName(fullName)
      case InterfaceRef2(fullName) => "𝕋" + getFullNameIdentifierName(fullName)
      case OverloadSet(env, name, voidStructRef) => {
        "𝔾" + " " + env + " " + name
      }
    }
  }

  private def getIdentifierName(tyype: ITemplata): String = {
    tyype match {
      case KindTemplata(referend) => "ㄊ" + getReferendIdentifierName(referend)
      case CoordTemplata(reference) => "ㄊ" + getReferenceIdentifierName(reference)
      case MutabilityTemplata(Mutable) => "ㄊmut"
      case MutabilityTemplata(Immutable) => "ㄊimm"
      case IntegerTemplata(num) => "ㄊ" + num
//      case StructTemplateTemplata(struct1) => "ㄊ𝕊" + struct1.struct1Id
//      case InterfaceTemplateTemplata(interface1) => "ㄊ𝕋" + interface1.interface1Id
    }
  }

  def getIdentifierName(prototype: Prototype2): String = {
    val Prototype2(fullName, FunctionT2(paramsTypes2, returnType2)) = prototype;
    "𝔽" + getFullNameIdentifierName(fullName) +
        "(" + paramsTypes2.map(p => getReferenceIdentifierName(p)).mkString(",") + ")"
  }
}
