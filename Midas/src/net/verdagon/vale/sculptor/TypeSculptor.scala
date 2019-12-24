package net.verdagon.vale.sculptor

import net.verdagon.vale.hammer._
import net.verdagon.vale.templar._

object TypeSculptor {
//  def getBaseLlvmType(tyype: BaseTypeH): String = {
//    println("ignoring owner!")
//    tyype match {
//      case a @ AddressibleH(_) => getAddressibleLlvmType(a)
//      case p : ReferenceH => getPointerLlvmType(p)
//    }
//  }

//  def getMemberTypeLlvmType(tyype: IMemberTypeH): String = {
//    tyype match {
//      case ReferenceMemberTypeH(reference) => getReferenceLlvmType(tyype.reference)
//      case AddressMemberTypeH(reference) => getReferenceLlvmType(tyype.reference) + "*"
//    }
//
//  }

  def getAddressRegisterLlvmType(tyype: AddressRegisterH): String = {
    getReferenceLlvmType(tyype.reference) + "*"
  }

  def getReferenceRegisterLlvmType(tyype: ReferenceRegisterH): String = {
    println("ignoring owner!")
    tyype match {
      case ReferenceRegisterH(ReferenceH(owning, innerType)) => getConcreteLlvmType(innerType) + "*"
    }
  }

  def getReferenceLlvmType(reference: ReferenceH[ReferendH]): String = {
    getConcreteLlvmType(reference.innerType) + "*"
  }

  def getConcreteLlvmType(tyype: ReferendH): String = {
    tyype match {
      case IntH() => "i64"
      case FloatH() => "double"
      case BoolH() => "i1"
      case StrH() => "%__Str"
      case VoidH() => "%__Void"
      case FunctionTH(paramTypes, returnType) => {
        // Note how this is not going to inner, this is so we can
        // do i64(%mystruct*)
        getReferenceLlvmType(returnType) + "(" + paramTypes.map(getReferenceLlvmType).mkString(", ") + ")"
      }
      case StructRefH(_, globalName) => {
        "%\"" + globalName + "\""
      }
      case InterfaceRefH(interfaceId, globalName) => {
        "%\"" + globalName + "\""
      }
    }
  }
//
//  def getFullName(tyype: PointerH): String = {
//    tyype match {
//      case PointerH(innerType) => {
//        "ref." + getFullName(innerType)
//      }
//    }
//  }
//
//  def getFullName(tyype: ConcreteValueH): String = {
//    tyype match {
//      case IntH() => "i64"
//      case FloatH() => "double"
//      case BoolH() => "i1"
//      case StrH() => "str"
//      case FunctionTH(paramTypes, returnType) => {
//        "func;" + getFullName(returnType) + ";" + paramTypes.size + paramTypes.map(getFullName).map(";" + _).mkString("")
//      }
//      case StructRefH(name) => name
//    }
//  }
//
//
//  def getFullName(tyype: Value2): String = {
//    if (tyype.isInstanceOf[Coord]) {
//      getFullName(tyype.asInstanceOf[Coord])
//    } else if (tyype.isInstanceOf[ConcreteValue2]) {
//      getFullName(tyype.asInstanceOf[ConcreteValue2])
//    } else {
//      vfail("wat")
//    }
//  }
//

  // Identifier names need to come from the Templar output because some things are erased
  // by Hammer types, such as template args. Hammer will sometimes output many functions
  // with the same signature because of this.

  def getReferenceIdentifierName(reference: Coord): String = {
    (reference.ownership match { case Borrow => "b" case Own => "o" case Raw => "r" }) +
        "ref:" + getReferendIdentifierName(reference.referend)
  }

  def getReferendIdentifierName(tyype: Kind): String = {
    tyype match {
      case Int2() => "i64"
      case Float2() => "double"
      case Bool2() => "i1"
      case Str2() => "__Str"
      case Void2() => "__Void"
      case FunctionT2(paramTypes, returnType) => {
        "func:" + getReferenceIdentifierName(returnType) + ":" + paramTypes.size + paramTypes.map(getReferenceIdentifierName).map(":" + _).mkString("")
      }
      case PackT2(innerTypes, underlyingStruct) => {
        getReferendIdentifierName(underlyingStruct)
//        "pack:" + memberTypes.size + memberTypes.map(getIdentifierName).map(":" + _).mkString("")
      }
      case StructRef2(name, templateArgTypes) => {
        "struct:" + name + ":" + templateArgTypes.size + templateArgTypes.map(getIdentifierName).map(":" + _).mkString("")
      }
      case InterfaceRef2(name, templateArgTypes) => {
        "interface:" + name + ":" + templateArgTypes.size + templateArgTypes.map(getIdentifierName).map(":" + _).mkString("")
      }
    }
  }
//
//  private def getIdentifierName(tyype: BaseType2): String = {
//    tyype match {
//      case Addressible2(pointerType) => {
//        "addressible:" + getIdentifierName(pointerType)
//      }
//      case v : Value2 => getValueIdentifierName(v)
//    }
//  }

  private def getIdentifierName(tyype: ITemplata): String = {
    tyype match {
      case ReferenceTemplata(tyype2) => getReferenceIdentifierName(tyype2)
      case ReferendTemplata(tyype2) => getReferendIdentifierName(tyype2)
//      case StructTemplateTemplata(struct1) => "s1id:" + struct1.struct1Id
//      case InterfaceTemplateTemplata(interface1) => "i1id:" + interface1.interface1Id
    }
  }

  def getIdentifierName(prototype: Prototype2): String = {
    val Prototype2(humanName, explicitTemplateArgs, FunctionT2(paramsTypes2, returnType2)) = prototype;
    val paramsStr = paramsTypes2.map(p => ":" + getReferenceIdentifierName(p)).mkString("")
    val explicitTemplateArgsStr = explicitTemplateArgs.map(p => ":" + getIdentifierName(p)).mkString(",")
    humanName + (":" + explicitTemplateArgs.size) + explicitTemplateArgsStr + (":" + paramsTypes2.size) + paramsStr
  }
}
