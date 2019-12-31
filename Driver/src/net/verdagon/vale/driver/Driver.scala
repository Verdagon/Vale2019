package net.verdagon.vale.driver

import net.verdagon.vale.astronomer.{Astronomer, ProgramA}
import net.verdagon.vale.carpenter.Carpenter
import net.verdagon.vale.hammer.{Hammer, VonHammer}
import net.verdagon.vale.parser.VParser
import net.verdagon.vale.scout.Scout
import net.verdagon.vale.templar.Templar
import net.verdagon.vale.{vassert, vassertSome}
import net.verdagon.von.{JsonSyntax, VonPrinter}

import scala.io.Source

object Driver {
  def main(args: Array[String]): Unit = {
    vassert(args.size >= 1)

    val bufferedSource = Source.fromFile(args(0))
    val code = bufferedSource.getLines.mkString
    bufferedSource.close

    val parsed = vassertSome(VParser.runParser(code))

    val scoutput = Scout.scoutProgram(parsed)

    val astrouts = Astronomer.runAstronomer(scoutput)

    val temputs = Templar.evaluate(astrouts)

    val hinputs = Carpenter.translate(temputs)

    val hamuts = Hammer.translate(hinputs)

    val programV = VonHammer.vonifyProgram(hamuts)

    val json = new VonPrinter(JsonSyntax, 120).print(programV)

    println(json)
  }
}