package net.verdagon.vale.highlighter

import net.verdagon.vale.parser._
import net.verdagon.vale.vfail

object Highlighter {
  def min(positions: List[Pos]): Pos = {
    positions match {
      case List() => vfail()
      case List(p) => p
      case head :: tail => {
        val minOfTail = min(tail)
        if (head < minOfTail) { head }
        else { minOfTail }
      }
    }
  }

  class CodeIter(code: String) {
    var index = 0
    var line = 1
    var col = 1
    private def pos = Pos(line, col)

    private def advance(): Unit = {
      index = index + 1
      col = col + 1
      if (code.charAt(index - 1) == '\n') {
        line = line + 1
        col = 1
      }
    }

    def advanceTo(untilPos: Pos): String = {
      val indexBefore = index
      while (pos < untilPos && index < code.length) {
        advance()
      }
      val indexAfter = index
      code.substring(indexBefore, indexAfter)
    }
  }

  def toHTML(builder: StringBuilder, iter: CodeIter, span: Span): String = {
    builder.append(iter.advanceTo(span.range.begin))
    builder.append(s"""<span class="${span.classs}">""")
    span.children.foreach(child => {
      builder.append(iter.advanceTo(child.range.begin))
      toHTML(builder, iter, child)
      builder.append(iter.advanceTo(child.range.end))
    })
    builder.append(iter.advanceTo(span.range.end))
    builder.append("</span>")
    builder.toString()
  }

  def toHTML(code: String, span: Span): String = {
    val iter = new CodeIter(code)
    val builder = new StringBuilder()
    toHTML(builder, iter, span)
  }
}
