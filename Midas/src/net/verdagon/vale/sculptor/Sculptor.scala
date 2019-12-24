package net.verdagon.vale.sculptor

import net.verdagon.vale.hammer.{PrototypeH, _}
import net.verdagon.vale.templar.{Borrow, Own}

import scala.collection.immutable.ListMap



object Sculptor {
  def translate(programH: ProgramH): String = {
    StructSculptor.declareStructs(programH) +
    programH.functions.map(f => translate(programH, f)).mkString("\n\n")
  }

  private def translate(programH: ProgramH, functionH: FunctionH): String = {
    val FunctionH(PrototypeH(prototype2, humanName, globalName, paramsH, returnTypeH), _, _, _, nodes) = functionH;

    val returnTypeStr = TypeSculptor.getReferenceLlvmType(returnTypeH);
    val paramsStr =
        paramsH.zipWithIndex
        .map({
          case (paramType, index) => TypeSculptor.getReferenceLlvmType(paramType) + " %arg" + index
        })
        .mkString(", ");
    val signature = returnTypeStr + " @\"" + globalName + "\"(" + paramsStr + ")";

    val body = translate(programH, nodes.nodes)

    "define " + signature + "{\n" +
    body +
    "}\n"
  }

  private def translate(programH: ProgramH, nodes: Vector[NodeH]): String = {

//  for (int nextLine = 0; nextLine < function->nodes.size(); ) {
//    auto & current = function->nodes[nextLine];
//    std::cout << "Translating " << current->toString() << endl;
//    nextLine++;

    nodes.map(n => translate(programH, n)).mkString("")
  }

  private def translate(programH: ProgramH, node: NodeH): String = {
    val saveStart = "%line" + node.registerId + " = ";

    val result =
        node match {
          case LoadFunctionH(line, functionRefH) => {
            // assuming name is "+"
            // i64(i64, i64)*
            println("hardcoding owning")
            val funcType = TypeSculptor.getReferenceLlvmType(ReferenceH(Borrow, functionRefH.functionType));
            // %line6 = bitcast i64(i64, i64)* @"+" to i64(i64, i64)*
            val globalName = functionRefH.globalName
            saveStart + "bitcast " + funcType + " @\"" + globalName + "\" to " + funcType + "\n"
          }
//          case LoadExternFunctionH(line, functionRefH) => {
//            // assuming name is "+"
//            // i64(i64, i64)*
//            println("hardcoding owning")
//            val funcType = TypeSculptor.getReferenceLlvmType(ReferenceH(Borrow, functionRefH.functionType));
//            val globalName = functionRefH.globalName
//            // %line6 = bitcast i64(i64, i64)* @"+" to i64(i64, i64)*
//            saveStart + "bitcast " + funcType + " @\"" + globalName + "\" to " + funcType + "\n"
//          }
          case ConstantBoolH(line, value) => {
            saveStart + "and i1 1, " + value + "\n"
          }
          case ConstantI64H(line, value) => {
            "%line" + line + "sizeptr = getelementptr i64, i64* null, i64 1\n" +
            "%line" + line + "size = ptrtoint i64* %line" + line + "sizeptr to i64\n" +
            "%line" + line + "raw = call i8* @malloc(i64 %line" + line + "size)\n" +
            "%line" + line + " = bitcast i8* %line" + line + "raw to i64*\n" +
            "%line" + line + "value = add i64 0, " + value + "\n" +
            "store i64 %line" + line + "value, i64* %line" + line + "\n"
          }
          case ConstantF64H(line, value) => {
            saveStart + "fadd double 0.0, " + value + "\n"
          }
          case ArgumentH(line, tyype, argumentIndex) => {
            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
            saveStart + "alloca " + typeName + "\n" +
            "store " + typeName + " %arg" + argumentIndex + ", " + typeName + "* %line" + line + "\n"
          }
          case StackifyH(line, sourceRegister, localIndex, name) => {
            val typeName = TypeSculptor.getReferenceLlvmType(sourceRegister.expectedType)
            saveStart + "alloca " + typeName + " ; NAME\n"
          }
//          case s @ StoreH(line, destinationRegister, sourceRegister) => {
//            val sourceTypeName = TypeSculptor.getReferenceLlvmType(sourceRegister.expectedType)
//            val destinationTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegisterH(destinationRegister.expectedType))
//            "store " + sourceTypeName + " %line" + sourceRegister.expectedType + ", " + destinationTypeName + " %line" + destinationRegister.expectedType + "\n"
//          }
//          case SoftLoadH(line, sourceRegister, targetOwnership) => {
//            val destinationTypeName = TypeSculptor.getReferenceLlvmType(sourceRegister.expectedType)
//            val sourceTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegisterH(sourceRegister.expectedType))
//            saveStart + "load " + destinationTypeName + ", " + sourceTypeName + " %line" + sourceRegister.expectedType + "\n"
//          }
//          case LocalLookupH(line, index, tyype, name) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
//            saveStart + "bitcast " + typeName + " %line" + tyype + " to " + typeName + " ; NAME\n"
//          }
//          case ExtractH(line, tyype, sourceLine, index) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "extractvalue " + typeName + " %line" + sourceLine + ", " + index + "\n"
//          }
//          case AddH(line, tyype, leftLine, rightLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "add " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case EqH(line, tyype, leftLine, rightLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "icmp eq " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case AndH(line, leftLine, rightLine) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(ReferenceH(Own, BoolH()))
//            // todo: dereference
//            saveStart + "and " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case OrH(line, leftLine, rightLine) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(ReferenceH(Own, BoolH()))
//            // todo: dereference
//            saveStart + "or " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
          case cH @ CallH(line, functionRegister, argsRegisters) => {
            val resultTypeName = TypeSculptor.getReferenceLlvmType(cH.resultType)
            saveStart + "call " + resultTypeName + " %line" + functionRegister.registerId + "(" +
            argsRegisters
                .map({
                  case RegisterAccessH(argRegisterId, argType) => TypeSculptor.getReferenceLlvmType(argType) + " %line" + argRegisterId
                })
                .mkString(", ") +
            ")\n"
          }
          case ReturnH(line, sourceRegister) => {
            val typeName = TypeSculptor.getReferenceLlvmType(sourceRegister.expectedType)
            "ret " + typeName + " %line" + sourceRegister.registerId + "\n"
          }
//          case BranchH(line, conditionLine, trueDestinationLine, falseDestinationLine) => {
//            "br i1 %line" + conditionLine + ", label %label" + trueDestinationLine + ", label %label" + falseDestinationLine + "\n"
//          }
//          case PhiH(line, tyype, cases) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
//            saveStart + "phi " + typeName + " " + cases.map(c => "[%line" + c.resultRegisterId + ", %label" + c.bodyLabelRegisterId + "]").mkString(", ") + "\n"
//          }
//          case JumpH(line, destinationLine) => {
//            "br label %label" + destinationLine + "\n"
//          }
//          case LabelH(line) => {
//            "label" + line + ":\n"
//          }
//          case DerefAndCopyH(line, resultType, sourceLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(resultType)
//            println("todo: copy structs here!")
//            saveStart + "load " + typeName + ", " + typeName + "* %line" + sourceLine + "\n"
//          }
//          case AddressMemberLookupH(line, sourceRegister, memberIndex, memberType) => {
//            val structTypeName = TypeSculptor.getConcreteLlvmType(sourceRegister.expectedType.innerType)
//            val memberTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegisterH(memberType))
//            val memberLlvmIndex = programH.structs.find(_.getRef == sourceRegister.expectedType.innerType).get.getMemberLlvmIndex(memberIndex)
//            "%line" + line + "ptr = getelementptr inbounds " + structTypeName + ", " + structTypeName + "* %line" + sourceRegister.registerId + ", i32 0, i32 " + memberLlvmIndex + "\n" +
//            saveStart + "load " + memberTypeName + ", " + memberTypeName + "* %line" + line + "ptr\n"
//          }
//          case StructLookupH(line, sourceRegister, memberIndex, memberType) => {
//            val structTypeName = TypeSculptor.getConcreteLlvmType(sourceRegister.expectedType.innerType)
//            val memberTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegisterH(memberType))
//            val memberLlvmIndex = programH.structs.find(_.getRef == memberType).get.getMemberLlvmIndex(memberIndex)
//            saveStart + "getelementptr inbounds " + structTypeName + ", " + structTypeName + "* %line" + sourceRegister.registerId + ", i32 0, i32 " + memberLlvmIndex + "\n"
//          }
          case NewStructH(line, sourceRegisters, structRefH) => {
            val llvmType = TypeSculptor.getConcreteLlvmType(structRefH.innerType)
            val typeName = structRefH.innerType.globalName
            val structDefH = programH.structs.find(_.getRef == structRefH).get;
            val sInfoPtrIndex = structDefH.getSInfoPtrElementIndex();
            "%line" + line + "sizeptr = getelementptr " + llvmType + ", " + llvmType + "* null, i64 1\n" +
                "%line" + line + "size = ptrtoint " + llvmType + "* %line" + line + "sizeptr to i64\n" +
                "%line" + line + "raw = call i8* @malloc(i64 %line" + line + "size)\n" +
                "%line" + line + " = bitcast i8* %line" + line + "raw to " + llvmType + "*\n" +
                "%line" + line + "vptrAddr = getelementptr " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + sInfoPtrIndex + "\n" +
                "store %__SInfo* getelementptr (%\"__SInfoWhole_" + typeName + "\", %\"__SInfoWhole_" + typeName + "\"* @\"__sInfoWhole_" + typeName + "\", i32 0, i32 1, i32 1), %__SInfo** %line" + line + "vptrAddr\n" +
                (structRefH match {
                  case s : StructRefH => {
                    val struct = programH.structs.find(_.getRef == s).get;
                    struct.members.zipWithIndex.zip(sourceRegisters.map(_.registerId)).map({
                      case _ => // impl me
//                      case ((StructMemberH(name, memberType), memberIndex), sourceLine) => {
//                        val index = struct.getMemberLlvmIndex(memberIndex)
//                        val memberTypeName = TypeSculptor.getMemberTypeLlvmType(memberType)
//                        "%line" + line + "member" + index + "ptr = getelementptr inbounds " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + index + "\n" +
//                            "store " + memberTypeName + " %line" + sourceLine + ", " + memberTypeName + "* %line" + line + "member" + index + "ptr\n"
//                      }
                    }).mkString("")
                  }
                })
          }
          case InterfaceToInterfaceUpcastH(line, sourceRegister, targetInterfaceRef) => {
            vfail("not yet")
          }
          case StructToInterfaceUpcastH(line, sourceRegister, targetInterfaceRef) => {
            val sourceStructDef = programH.structs.find(_.getRef == sourceRegister.expectedType).get;
            val structInfoPointerIndex = sourceStructDef.getSInfoPtrElementIndex();
            val interfaceElementIndex = sourceStructDef.getInterfacePtrElementIndex(targetInterfaceRef);
            val interfaceId = programH.interfaces.find(_.getRef == targetInterfaceRef).get.interfaceId;

            val sourceConcreteTypeName = TypeSculptor.getConcreteLlvmType(sourceRegister.expectedType.innerType)
            val resultTypeName = targetInterfaceRef.globalName
            "%line" + line + "sInfoEndPtrPtr = getelementptr " + sourceConcreteTypeName + ", " + sourceConcreteTypeName + "* %line" + sourceRegister.registerId + ", i32 0, i32 " + structInfoPointerIndex + "\n" +
            "%line" + line + "sInfoEndPtr = load %__SInfo*, %__SInfo** %line" + line + "sInfoEndPtrPtr\n" +
            "call void @__checkInterfaceCast(%__SInfo* %line" + line + "sInfoEndPtr, i32 " + interfaceId + ", %__IInfo* @\"__iinfo_" + resultTypeName + "\")\n" +
            "%line" + line + " = getelementptr " + sourceConcreteTypeName + ", " + sourceConcreteTypeName + "* %line" + sourceRegister.registerId + ", i32 0, i32 " + interfaceElementIndex + "\n"
          }
          case InterfaceCallH(line, virtualParamIndex, interfaceRefH, interfaceId, indexInedge, argLines, paramTypes) => {
            "\n"
          }
//          case InterfaceMethodLookupH(line, resultType, interfaceRefH, interfaceLine, interfaceId, indexInEdge) => {
//            val interfaceName = interfaceRefH.globalName
//            val interfaceConcreteTypeName = TypeSculptor.getConcreteLlvmType(interfaceRefH)
//            val methodTypeName = TypeSculptor.getReferenceLlvmType(resultType)
//            "%line" + line + "sInfoEndPtrPtr = getelementptr " + interfaceConcreteTypeName + ", " + interfaceConcreteTypeName + "* %line" + interfaceLine + ", i32 0, i32 0, i32 0\n" +
//            "%line" + line + "sInfoEndPtr = load %__SInfo*, %__SInfo** %line" + line + "sInfoEndPtrPtr\n" +
//            "%line" + line + "generalEdgePtr = call %__Edge*(%__SInfo*, i32) @__getEdge(%__SInfo* %line" + line + "sInfoEndPtr, i32 " + interfaceId + ")\n" +
//            "%line" + line + "specificEdgePtr = bitcast %__Edge* %line" + line + "generalEdgePtr to %\"__Edge_" + interfaceName + "\"*\n"+
//            "%line" + line + "methodPtrAddr = getelementptr %\"__Edge_" + interfaceName + "\", %\"__Edge_" + interfaceName + "\"* %line" + line + "specificEdgePtr, i32 0, i32 " + (indexInEdge + 1) + "\n" +
//            "%line" + line + " = load " + methodTypeName + ", " + methodTypeName + "* %line" + line + "methodPtrAddr\n"
//          }
          case PanicH(line) => {
            "call void @exit(i32 7)\n" +
            "unreachable\n"
          }
        };
    vassert(result.endsWith("\n"), "doesnt end with newline: " + result);
    result
  }
}