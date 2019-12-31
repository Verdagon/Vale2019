package net.verdagon.vale.templar

import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._

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
      }
    ownershipString + getReferendIdentifierName(referend)
  }

  def getFullNameIdentifierName(fullName: FullName2): String = {
    fullName.steps.map({
      case NamePart2(humanName, maybeTemplateArgs, maybeParameters, maybeCodeLocation) => {
        humanName +
          (maybeTemplateArgs match {
            case None => ""
            case Some(templateArgs) => "<" + templateArgs.map(templateArg => getIdentifierName(templateArg)).mkString(", ") + ">"
          }) +
          (maybeParameters match {
            case None => ""
            case Some(parameters) => "(" + parameters.map(parameter => getReferenceIdentifierName(parameter)).mkString(", ") + ")"
          }) +
          (maybeCodeLocation match {
            case None => ""
            case Some(CodeLocation2(file, line, char)) => "@" + file + ":" + line + ":" + char
          })
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
    val Prototype2(fullName, paramsTypes2, returnType2) = prototype;
    "𝔽" + getFullNameIdentifierName(fullName) +
        "(" + paramsTypes2.map(p => getReferenceIdentifierName(p)).mkString(",") + ")" +
        getReferenceIdentifierName(returnType2)
  }

  def getIdentifierName(paramFilter: ParamFilter): String = {
    val ParamFilter(tyype, virtuality) = paramFilter
    getReferenceIdentifierName(tyype) +
      (virtuality match {
        case None => ""
        case Some(Abstract2) => " abstract"
        case Some(Override2(kind)) => " impl " + getReferendIdentifierName(kind)
      })
  }
}
