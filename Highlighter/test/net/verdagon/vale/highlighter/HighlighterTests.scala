package net.verdagon.vale.highlighter
import net.verdagon.vale.highlighter.Spanner._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class HighlighterTests extends FunSuite with Matchers {
  private def compile(code: String): Program0 = {
    VParser.parse(VParser.program, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(program0, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        program0
      }
    }
  }

  test("Highlighter simple function") {
    val code =
      """
        |fn main() {
        |  3
        |}
        |""".stripMargin
    val program1 = compile(code)
    Highlighter.toHTML(code, Spanner.forProgram(program1)) shouldEqual
      s"""<span class="Prog">
         |<span class="Fn">fn <span class="FnName">main</span><span class="Params">()</span> {
         |  <span class="Block"><span class="Num">3</span></span>
         |}</span>
         |</span>""".stripMargin
  }
}
