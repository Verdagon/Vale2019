package net.verdagon.von

import net.verdagon.radonc.{vfail, vimpl}
import org.apache.commons.lang.StringEscapeUtils

sealed trait ISyntax
case object VonSyntax extends ISyntax
case object JsonSyntax extends ISyntax

class VonPrinter(syntax: ISyntax, lineWidth: Int) {
  def print(data: IVonData): String = {
    printSingleLine(data, lineWidth) match {
      case Some(str) => str
      case None => printMultiline(data, 0)
    }
  }

  def printMultiline(
    data: IVonData,
    indentation: Int):
  // None if we failed to put it on the one line.
  String = {
    data match {
      case VonInt(value) => value.toString
      case VonBool(value) => value.toString
      case VonStr(value) => {
        syntax match {
          case VonSyntax => "\"" + StringEscapeUtils.escapeJava(value) + "\""
          case JsonSyntax => "\"" + StringEscapeUtils.escapeJavaScript(value) + "\""
        }
      }
      case VonReference(id) => vimpl()
      case o @ VonObject(_, _, _) => printObjectMultiline(o, indentation)
      case a @ VonArray(_, _) => printArrayMultiline(a, indentation)
      case VonListMap(id, members) => vimpl()
      case VonMap(id, members) => vimpl()
    }
  }

  def printObjectMultiline(obbject: VonObject, indentation: Int): String = {
    val VonObject(tyype, None, members) = obbject

    printObjectStart(tyype, members.nonEmpty) + "\n" + members.zipWithIndex.map({ case (member, index) =>
      val memberStr =
        printMemberSingleLine(member, lineWidth) match {
          case None => printMemberMultiline(member, indentation + 1)
          case Some(s) => s
        }
      "  ".repeat(indentation + 1) + memberStr + (if (index == members.size - 1) "" else ",")
    }).mkString("\n") + printObjectEnd()
  }

  def printObjectStart(tyype: String, hasMembers: Boolean): String = {
    syntax match {
      case VonSyntax => tyype + "("
      case JsonSyntax => {
        "{__type: " + "\"" + StringEscapeUtils.escapeJavaScript(tyype) + "\"" + (if (hasMembers) ", " else "")
      }
    }
  }
  def printObjectEnd(): String = {
    syntax match {
      case VonSyntax => ")"
      case JsonSyntax => "}"
    }
  }

  def printArrayStart(): String = {
    syntax match {
      case VonSyntax => "List("
      case JsonSyntax => "["
    }
  }
  def printArrayEnd(): String = {
    syntax match {
      case VonSyntax => ")"
      case JsonSyntax => "]"
    }
  }

  def printArrayMultiline(array: VonArray, indentation: Int): String = {
    val VonArray(None, members) = array

    printArrayStart() + "\n" + members.zipWithIndex.map({ case (member, index) =>
      val memberStr =
        printSingleLine(member, lineWidth) match {
          case None => printMultiline(member, indentation + 1)
          case Some(s) => s
        }
      "  ".repeat(indentation + 1) + memberStr + (if (index == members.size - 1) "" else ",")
    }).mkString("\n") + printArrayEnd()
  }

  def printMemberMultiline(
    member: VonMember,
    indentation: Int):
  // None if we failed to put it on the one line.
  String = {
    getMemberPrefix(member) + printMultiline(member.value, indentation)
  }

  def getMemberPrefix(member: VonMember):
  // None if we failed to put it on the one line.
  String = {
    val VonMember(memberIndex, fieldName, _) = member

    (memberIndex.map(_.toString).toList ++ fieldName.toList)
      .reduceLeftOption(_ + " " + _) // None or Some("7") or Some("myField") or Some("7 myField")
      .map(printMemberPrefix) // None or Some("7 = ") or Some("myField = ") or Some("7 myField = ")
      .getOrElse("") // "" or "7 = " or "myField = " or "7 myField = "
  }


  def printSingleLine(
    data: IVonData,
    // None means it will get its own line.
    // Some(n) means we should only try to print to n characters then give up.
    lineWidthRemaining: Int):
  // None if we failed to put it on the one line.
  Option[String] = {
    data match {
      case VonInt(value) => Some(value.toString)
      case VonBool(value) => Some(value.toString)
      case VonStr(value) => {
        Some(
          syntax match {
            case VonSyntax => "\"" + StringEscapeUtils.escapeJava(value) + "\""
            case JsonSyntax => "\"" + StringEscapeUtils.escapeJavaScript(value) + "\""
          })
      }
      case VonReference(id) => vimpl()
      case o @ VonObject(_, _, _) => printObjectSingleLine(o, lineWidthRemaining)
      case a @ VonArray(_, _) => printArraySingleLine(a, lineWidthRemaining)
      case VonListMap(id, members) => vimpl()
      case VonMap(id, members) => vimpl()
    }
  }

  def printObjectSingleLine(
    obbject: VonObject,
    lineWidthRemainingForWholeObject: Int):
    // None if we failed to put it on the one line.
  Option[String] = {
    val VonObject(tyype, None, members) = obbject

    val prefix = printObjectStart(tyype, members.nonEmpty)

    val lineWidthRemainingAfterPrefix = lineWidthRemainingForWholeObject - prefix.length
    if (lineWidthRemainingAfterPrefix <= 0) {
      // identifier took up too much space, bail!
      None
    } else {
      // None means we failed to put it within lineWidthRemaining
      val initialObjectStr: Option[String] = Some(prefix)
      val maybeMembersStr =
        members.zipWithIndex.foldLeft((initialObjectStr))({
          case (None, _) => None
          case (Some(objectStrSoFar), (member, index)) => {
            val lineWidthRemaining = lineWidthRemainingForWholeObject - objectStrSoFar.length
            // If we get here, we're trying to fit things on one line.
            printMemberSingleLine(member, lineWidthRemaining) match {
              case None => None
              case Some(memberStr) => {
                val memberStrMaybeWithComma = memberStr + (if (index == members.size - 1) "" else ", ")
                if (memberStrMaybeWithComma.length > lineWidthRemaining) {
                  None
                } else {
                  Some(objectStrSoFar + memberStrMaybeWithComma)
                }
              }
            }
          }
        })
      maybeMembersStr match {
        case None => None
        case Some(membersStr) => {
          val wholeObjectStr = membersStr + printObjectEnd()
          if (wholeObjectStr.length > lineWidthRemainingForWholeObject) {
            None
          } else {
            Some(wholeObjectStr)
          }
        }
      }
    }
  }

  def printArraySingleLine(
    array: VonArray,
    lineWidthRemainingForWholeObject: Int):
  // None if we failed to put it on the one line.
  Option[String] = {
    val VonArray(id, members) = array

    val prefix = printArrayStart()

    val lineWidthRemainingAfterPrefix = lineWidthRemainingForWholeObject - prefix.length
    if (lineWidthRemainingAfterPrefix <= 0) {
      // identifier took up too much space, bail!
      None
    } else {
      // None means we failed to put it within lineWidthRemaining
      val initialObjectStr: Option[String] = Some(prefix)
      val maybeMembersStr =
        members.zipWithIndex.foldLeft((initialObjectStr))({
          case (None, _) => None
          case (Some(objectStrSoFar), (member, index)) => {
            val lineWidthRemaining = lineWidthRemainingForWholeObject - objectStrSoFar.length
            // If we get here, we're trying to fit things on one line.
            printSingleLine(member, lineWidthRemaining) match {
              case None => None
              case Some(memberStr) => {
                val memberStrMaybeWithComma = memberStr + (if (index == members.size - 1) "" else ", ")
                if (memberStrMaybeWithComma.length > lineWidthRemaining) {
                  None
                } else {
                  Some(objectStrSoFar + memberStr)
                }
              }
            }
          }
        })
      maybeMembersStr match {
        case None => None
        case Some(membersStr) => {
          val wholeObjectStr = membersStr + printArrayEnd()
          if (wholeObjectStr.length > lineWidthRemainingForWholeObject) {
            None
          } else {
            Some(wholeObjectStr)
          }
        }
      }
    }
  }

  def printMemberPrefix(name: String): String = {
    syntax match {
      case VonSyntax => name + " = "
      case JsonSyntax => name + ": "
    }
  }

  def printMemberSingleLine(
    member: VonMember,
    // None means it will get its own line.
    // Some(n) means we should only try to print to n characters then give up.
    lineWidthRemaining: Int):
  // None if we failed to put it on the one line.
  Option[String] = {
    val VonMember(memberIndex, fieldName, value) = member

    val identifier =
      (memberIndex.map(_.toString).toList ++ fieldName.toList)
        .reduceLeftOption(_ + " " + _) // None or Some("7") or Some("myField") or Some("7 myField")
        .map(printMemberPrefix) // None or Some("7 = ") or Some("myField = ") or Some("7 myField = ")
        .getOrElse("") // "" or "7 = " or "myField = " or "7 myField = "

    val lineWidthRemainingForValue = lineWidthRemaining - identifier.length
    if (lineWidthRemainingForValue <= 0) {
      // identifier took up too much space, bail!
      None
    } else {
      printSingleLine(value, lineWidthRemainingForValue) match {
        case None => {
          // We failed to put it in the remaining space, which means the entire member string is too long.
          None
        }
        case Some(valueStr) => {
          val result = identifier + valueStr
          if (result.length >= lineWidthRemaining) {
            None
          } else {
            Some(result)
          }
        }
      }
    }
  }

}
