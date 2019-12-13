package net.verdagon.vale.sculptor

import net.verdagon.vale.carpenter.DirectoryEntry
import net.verdagon.vale.hammer._

import scala.collection.immutable.ListMap

object StructSculptor {

  private def signatureToString(paramTypes: List[Reference3[Referend3]], returnType3: Reference3[Referend3]): String = {
    val returnTypeStr = TypeSculptor.getReferenceLlvmType(returnType3);
    val paramsStr = paramTypes.map(TypeSculptor.getReferenceLlvmType).mkString(", ");
    returnTypeStr + " (" + paramsStr + ")*"
  }

  private def functionRefToLlvmFunctionRef(functionRef: FunctionRef3): String = {
    // void ()* @"doTheThingNAME"
    signatureToString(functionRef.prototype.params, functionRef.prototype.returnType) + "@\"" + functionRef.globalName + "\""
  }

  def declareStructs(program: Program3): String = {
    val structs = program.structs
    val interfaces = program.interfaces
    interfaces.map(interface => declareInterfaceInfo(program, interface)).mkString("") + "\n" +
    structs.map(struct => declareStructInfo(program, struct)).mkString("") + "\n"
  }

  private def declareInterfaceInfo(program: Program3, interface: InterfaceDefinition3): String = {
    val interfaceName = interface.globalName
    val interfaceNameLength = (interfaceName.length + 1).toString
    val methods = interface.prototypes.map(method => {
      """  , SIGNATURETYPE ; PROTOTYPENAME"""
          .replaceAllLiterally("SIGNATURETYPE", signatureToString(method.params, method.returnType))
          .replaceAllLiterally("PROTOTYPENAME", method.humanName)
    }).mkString("\n")

    """
      |@"__iname_INAME" = linkonce_odr constant [ILEN x i8] c"INAME\00"
      |@"__iinfo_INAME" = constant %__IInfo {
      |  i32 INTERFACEID, ; interface ID
      |  i8* getelementptr inbounds ([ILEN x i8], [ILEN x i8]* @"__iname_INAME", i32 0, i32 0)
      |}
      |
      |%"INAME" = type {
      |  [0 x %__SInfo*]
      |}
      |
      |%"__Edge_INAME" = type {
      |  %__Edge
      |  ; methods go below here
      |METHODS
      |}
      |
    """.stripMargin
        .replaceAllLiterally("INAME", interfaceName)
        .replaceAllLiterally("ILEN", interfaceNameLength)
        .replaceAllLiterally("INTERFACEID", interface.interfaceId.toString)
        .replaceAllLiterally("METHODS", methods)
  }

  private def getKTablePath(program: Program3, struct: StructDefinition3, interface: InterfaceRef3): List[Int] = {
    val numEdges = struct.edges.size;
    val numIntroducedMethods = struct.methods.size;

    val foundEdgeIndexMaybe = struct.edges.indexWhere(edge => edge.interface == interface)
    foundEdgeIndexMaybe match {
      case -1 => {
        struct.base match {
          case None => vfail("wat")
          case Some(baseStruct) => {
            val indexOfBaseVTable = numEdges + numIntroducedMethods;
            val baseStructDef = program.structs.filter(_.getRef == baseStruct).head;
            val coordInBase = getKTablePath(program, baseStructDef, interface)
            indexOfBaseVTable :: coordInBase
          }
        }
      }
      case foundEdgeIndex => {
        List(foundEdgeIndex)
      }
    }
  }

  private def assembleETableDirectory(program: Program3, struct: StructDefinition3): (String, String) = {
    val structName = struct.globalName

    val (types, values) =
      struct.eTable.table.directory.zipWithIndex.map({
        case (None, direntryIndex) => {
          val direntryType = "%__EEmptyDirentry,"

          val direntryValue =
            """
              |    %__EEmptyDirentry {
              |      %__EEmptyDirentry* getelementptr (%"__SInfoWhole_SNAME", %"__SInfoWhole_SNAME"* @"__sInfoWhole_SNAME", i32 0, i32 2, i32 DIRENTRYINDEX),
              |      i32 1
              |    },
            """.stripMargin
                .replaceAllLiterally("SNAME", structName)
                .replaceAllLiterally("DIRENTRYINDEX", direntryIndex.toString);

          (direntryType, direntryValue)
        }
        case (Some(DirectoryEntry(bucketStartIndex, bucketSize)), direntryIndex) => {
          val direntryType = "%__EDirentry,"

          val direntryValue =
            """
              |    %__EDirentry {
              |      %__Edge** getelementptr (%"__SInfoWhole_SNAME", %"__SInfoWhole_SNAME"* @"__sInfoWhole_SNAME", i32 0, i32 3, i32 BUCKETSTARTINDEX),
              |      i32 BUCKETSIZE
              |    },
            """.stripMargin
                .replaceAllLiterally("SNAME", structName)
                .replaceAllLiterally("BUCKETSIZE", bucketSize.toString)
                .replaceAllLiterally("BUCKETSTARTINDEX", bucketStartIndex.toString);

          (direntryType, direntryValue)
        }
      }).unzip

    (types.mkString("\n"), values.mkString("\n"))
  }

  private def assembleETableForest(program: Program3, struct: StructDefinition3): (String, String) = {
    val structName = struct.globalName

    val (types, values) =
      struct.eTable.table.combinedBuckets.zipWithIndex.map({
        case (None, forentryIndex) => {
          val forentryType = "%__EEmptyForentry,"

          val forentryValue =
            """
              |    %__EEmptyForentry {
              |      %__EEmptyForentry* getelementptr (%"__SInfoWhole_SNAME", %"__SInfoWhole_SNAME"* @__sInfoWhole_SNAME, i32 0, i32 4, i32 FORENTRYINDEX)
              |    },
            """.stripMargin
                .replaceAllLiterally("SNAME", structName)
                .replaceAllLiterally("FORENTRYINDEX", forentryIndex.toString);

          (forentryType, forentryValue)
        }
        case (Some((interfaceRef3, _)), direntryIndex) => {
          val interfaceName = interfaceRef3.globalName
          val direntryType = """  %"__Edge_INAME"*,"""
              .replaceAllLiterally("INAME", interfaceName)

          val path = getKTablePath(program, struct, interfaceRef3).map("i32 " + _).mkString(", ")
          val direntryValue =
            """    %"__Edge_INAME"* getelementptr (%"__SInfoWhole_SNAME", %"__SInfoWhole_SNAME"* @"__sInfoWhole_SNAME", i32 0, i32 0, PATH),"""
                .replaceAllLiterally("INAME", interfaceName)
                .replaceAllLiterally("SNAME", structName)
                .replaceAllLiterally("PATH", path)

          (direntryType, direntryValue)
        }
      }).unzip

    (types.mkString("\n"), values.mkString("\n"))
  }

  private def declareStructInfo(program: Program3, struct: StructDefinition3): String = {
    val (eTableDirectoryType, eTableDirectoryValue) = assembleETableDirectory(program, struct)
    val (eTableForestType, eTableForestValue) = assembleETableForest(program, struct)

    val structName = struct.globalName
    val members =
      struct.members.map(m => "  , " + TypeSculptor.getReferenceLlvmType(m.tyype) + " ; " + m.name).mkString("\n")

    val (methodsType, methodsValue) = assembleMethodsParameter(program, struct, struct)

    val emptyInterfaces = struct.edges.map(edge => {
      """  %"INAME", ; zero-length, for upcasting"""
          .replace("INAME", edge.interface.globalName)
    }).mkString("\n")

    """
      |%"NAME" = type {
      |EMPTY_INTERFACES
      |  [0 x %__SInfo*],
      |  %__SInfo*
      |MEMBERS
      |}
      |
      |METHODS_TYPE
      |
      |%"__EDirectory_NAME" = type {
      |EDIRENTRIESTYPES
      |  %__IgnoreMe ; because LLVM doesn't like trailing commas...
      |}
      |
      |%"__EForest_NAME" = type {
      |EFORENTRIESTYPES
      |  %__IgnoreMe ; because LLVM doesn't like trailing commas...
      |}
      |
      |%"__SInfoWhole_NAME" = type {
      |  %"__Methods_NAME",
      |  [1 x %__SInfo],
      |  %"__EDirectory_NAME",
      |  [0 x %__Edge*],
      |  %"__EForest_NAME"
      |}
      |
      |@"__sname_NAME" = linkonce_odr constant [NSTRLEN x i8] c"NAME\00"
      |@"__sInfoWhole_NAME" = constant %"__SInfoWhole_NAME" {
      |METHODS_VALUE,
      |  [1 x %__SInfo] [
      |    %__SInfo {
      |      [0 x i11*] [],
      |      i8* getelementptr inbounds ([NSTRLEN x i8], [NSTRLEN x i8]* @"__sname_NAME", i32 0, i32 0),
      |      %__SInfo* null,
      |      i32 EDIRECTORYSIZEMASK,
      |      [0 x %__EDirentry] []
      |    }
      |  ],
      |  %"__EDirectory_NAME" {
      |EDIRENTRIESVALUES
      |    %__IgnoreMe {} ; because LLVM doesn't like trailing commas...
      |  },
      |  [0 x %__Edge*] [],
      |  %"__EForest_NAME" {
      |EFORENTRIESVALUES
      |    %__IgnoreMe {} ; because LLVM doesn't like trailing commas...
      |  }
      |}
    """.stripMargin
        .replaceAllLiterally("NAME", structName)
        .replaceAllLiterally("NSTRLEN", (structName.length + 1).toString)
        .replaceAllLiterally("EMPTY_INTERFACES", emptyInterfaces)
        .replaceAllLiterally("MEMBERS", members)
        .replaceAllLiterally("EDIRENTRIESTYPES", eTableDirectoryType)
        .replaceAllLiterally("EDIRENTRIESVALUES", eTableDirectoryValue)
        .replaceAllLiterally("EFORENTRIESTYPES", eTableForestType)
        .replaceAllLiterally("EFORENTRIESVALUES", eTableForestValue)
        .replaceAllLiterally("EDIRECTORYSIZEMASK", (struct.eTable.table.directory.length - 1).toString)
        .replaceAllLiterally("METHODS_TYPE", methodsType)
        .replaceAllLiterally("METHODS_VALUE", methodsValue)
  }

  private def assembleMethodsInterfaceEdges(program: Program3, struct: StructDefinition3): (String, String) = {
    val (types, values) =
      struct.edges.map(edge => {
        val interfaceName = edge.interface.globalName
        val tyype = """  %"__Edge_INAME","""
            .replaceAllLiterally("INAME", interfaceName)

        val introducedMethods =
          edge.structPrototypesByInterfacePrototype.map({
            case (interfacePrototype, structPrototype) => {
              """      ,  ISIGNATURE bitcast (ASIGNATURE @"METHODID" to ISIGNATURE) ; PROTOTYPENAME"""
                  .replaceAllLiterally("PROTOTYPENAME", interfacePrototype.humanName)
                  .replaceAllLiterally("ISIGNATURE", signatureToString(interfacePrototype.params, interfacePrototype.returnType))
                  .replaceAllLiterally("ASIGNATURE", signatureToString(structPrototype.params, structPrototype.returnType))
                  .replaceAllLiterally("METHODID", structPrototype.globalName)
            }
          }).mkString("\n")

        val value =
          """
            |    %"__Edge_INAME" {
            |      %__Edge {
            |        %__IInfo* @"__iinfo_INAME"
            |      }
            |METHODS
            |    },
          """.stripMargin
            .replaceAllLiterally("INAME", interfaceName)
            .replaceAllLiterally("METHODS", introducedMethods)

        (tyype, value)
      }).unzip
    (types.mkString("\n"), values.mkString("\n"))
  }

  private def assembleMethodsBaseClass(program: Program3, struct: StructDefinition3): (String, String) = {
    val superclassMethodsType =
      struct.base match {
        case None => ""
        case Some(StructRef3(_, globalName)) => {
          """  %"__Methods_SUPERSTRUCTID","""
              .replaceAllLiterally("SUPERSTRUCTID", globalName)
        }
      }
    val superclassMethodsValue =
      struct.base match {
        case None => ""
        case Some(superstructRef) => {
          val superstructDef = program.structs.filter(_.getRef == superstructRef).head;
          val (superclassMethodsValue, _) = assembleMethodsParameter(program, superstructDef, struct)
          superclassMethodsValue
        }
      }
    (superclassMethodsType, superclassMethodsValue)
  }

  private def assembleIntroducedMethods(program: Program3, struct: StructDefinition3): (String, String) = {
    val (introducedMethodsType, introducedMethodsValue) =
      struct.methods.map(method => {
        val signatureTypeLine = """  SIGNATURETYPE, ; PROTOTYPENAME"""
            .replaceAllLiterally("SIGNATURETYPE", signatureToString(method.prototype.params, method.prototype.returnType))
            .replaceAllLiterally("PROTOTYPENAME", method.prototype.humanName)

        val methodValueLine = """  SIGNATURETYPE @"METHODID", ; PROTOTYPENAME"""
            .replaceAllLiterally("SIGNATURETYPE", signatureToString(method.prototype.params, method.prototype.returnType))
            .replaceAllLiterally("METHODID", method.prototype.globalName)
            .replaceAllLiterally("PROTOTYPENAME", method.prototype.humanName)

        (signatureTypeLine, methodValueLine)
      }).unzip
    (introducedMethodsType.mkString("\n"), introducedMethodsValue.mkString("\n"))
  }

  private def assembleMethodsParameter(program: Program3, baseOrThisStruct: StructDefinition3, thisStruct: StructDefinition3): (String, String) = {
    val structName = thisStruct.globalName

    val (introducedMethodsTypes, introducedMethodsValues) =
      assembleIntroducedMethods(program, thisStruct)
    val (superclassMethodsType, superclassMethodsValue) =
      assembleMethodsBaseClass(program, thisStruct)
    val (interfaceEdgesTypes, interfaceEdgesValues) =
      assembleMethodsInterfaceEdges(program, thisStruct)

    val tyype =
      """
        |%"__Methods_NAME" = type {
        |  ; NAME interface edges
        |INTERFACE_EDGES_TYPE
        |  ; NAME introduced methods
        |INTRODUCED_METHODS_TYPE
        |  ; NAME superclass might go here
        |SUPERCLASS_METHODS_TYPE
        |  %__IgnoreMe ; because LLVM doesn't like trailing commas...
        |}
      """.stripMargin
        .replaceAllLiterally("NAME", structName)
        .replaceAllLiterally("INTRODUCED_METHODS_TYPE", introducedMethodsTypes)
        .replaceAllLiterally("INTERFACE_EDGES_TYPE", interfaceEdgesTypes)
        .replaceAllLiterally("SUPERCLASS_METHODS_TYPE", superclassMethodsType)

    val valueTemplate =
      """
        |  %"__Methods_NAME" {
        |    ; NAME interface edges
        |INTERFACE_EDGES_VALUE
        |    ; NAME introduced methods
        |INTRODUCED_METHODS_VALUE
        |    ; NAME superclass might go here
        |SUPERCLASS_METHODS_VALUE
        |    %__IgnoreMe {} ; because LLVM doesn't like trailing commas...
        |  }
      """.stripMargin
          .replaceAllLiterally("NAME", structName)
          .replaceAllLiterally("INTERFACE_EDGES_VALUE", interfaceEdgesValues)
          .replaceAllLiterally("INTRODUCED_METHODS_VALUE", introducedMethodsValues)
          .replaceAllLiterally("SUPERCLASS_METHODS_VALUE", superclassMethodsValue)

    (tyype, valueTemplate)
  }
}
