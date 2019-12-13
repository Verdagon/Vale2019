package net.verdagon.vale.sculptor

import net.verdagon.vale.hammer._
import net.verdagon.vale.templar._

object TypeSculptor {
//  def getBaseLlvmType(tyype: BaseType3): String = {
//    println("ignoring owner!")
//    tyype match {
//      case a @ Addressible3(_) => getAddressibleLlvmType(a)
//      case p : Reference3 => getPointerLlvmType(p)
//    }
//  }

//  def getMemberTypeLlvmType(tyype: IMemberType3): String = {
//    tyype match {
//      case ReferenceMemberType3(reference) => getReferenceLlvmType(tyype.reference)
//      case AddressMemberType3(reference) => getReferenceLlvmType(tyype.reference) + "*"
//    }
//
//  }

  def getAddressRegisterLlvmType(tyype: AddressRegister3): String = {
    getReferenceLlvmType(tyype.reference) + "*"
  }

  def getReferenceRegisterLlvmType(tyype: ReferenceRegister3): String = {
    println("ignoring owner!")
    tyype match {
      case ReferenceRegister3(Reference3(owning, innerType)) => getConcreteLlvmType(innerType) + "*"
    }
  }

  def getReferenceLlvmType(reference: Reference3[Referend3]): String = {
    getConcreteLlvmType(reference.innerType) + "*"
  }

  def getConcreteLlvmType(tyype: Referend3): String = {
    tyype match {
      case Int3() => "i64"
      case Float3() => "double"
      case Bool3() => "i1"
      case Str3() => "%__Str"
      case Void3() => "%__Void"
      case FunctionT3(paramTypes, returnType) => {
        // Note how this is not going to inner, this is so we can
        // do i64(%mystruct*)
        getReferenceLlvmType(returnType) + "(" + paramTypes.map(getReferenceLlvmType).mkString(", ") + ")"
      }
      case StructRef3(_, globalName) => {
        "%\"" + globalName + "\""
      }
      case InterfaceRef3(interfaceId, globalName) => {
        "%\"" + globalName + "\""
      }
    }
  }
//
//  def getFullName(tyype: Pointer3): String = {
//    tyype match {
//      case Pointer3(innerType) => {
//        "ref." + getFullName(innerType)
//      }
//    }
//  }
//
//  def getFullName(tyype: ConcreteValue3): String = {
//    tyype match {
//      case Int3() => "i64"
//      case Float3() => "double"
//      case Bool3() => "i1"
//      case Str3() => "str"
//      case FunctionT3(paramTypes, returnType) => {
//        "func;" + getFullName(returnType) + ";" + paramTypes.size + paramTypes.map(getFullName).map(";" + _).mkString("")
//      }
//      case StructRef3(name) => name
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
