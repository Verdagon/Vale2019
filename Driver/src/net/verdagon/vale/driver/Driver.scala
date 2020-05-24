package net.verdagon.vale.driver

import java.io.{OutputStream, PrintStream}
import java.util.InputMismatchException

import net.verdagon.vale.astronomer.{Astronomer, ProgramA}
import net.verdagon.vale.carpenter.Carpenter
import net.verdagon.vale.hammer.{Hammer, VonHammer}
import net.verdagon.vale.metal.ProgramH
import net.verdagon.vale.parser.VParser
import net.verdagon.vale.scout.Scout
import net.verdagon.vale.templar.Templar
import net.verdagon.vale.vivem.Vivem
import net.verdagon.vale.samples.Roguelike
import net.verdagon.vale.{MainRetAdd, OrdinaryLinkedList, Sum, Terrain, vassert, vassertSome, vcheck}
import net.verdagon.von.{IVonData, JsonSyntax, VonInt, VonPrinter}

import scala.io.Source

object Driver {
  case class InputException(message: String) extends Throwable

  def readCode(path: String): String = {
    if (path.startsWith("sample:")) {
      path.toLowerCase().slice("sample:".length, path.length) match {
        case "roguelike" => Roguelike.code
        case "terrain" => Terrain.generatorCode
        case "linkedlist" => OrdinaryLinkedList.code
        case "mainretadd" => MainRetAdd.code
        case "sum" => Sum.code
        case other => throw InputException("Unknown sample: " + other)
      }
    } else {
      val file = path
      val bufferedSource = Source.fromFile(file)
      val code = bufferedSource.getLines.mkString
      bufferedSource.close
      code
    }
  }

  def build(code: String): ProgramH = {
    val parsed = vassertSome(VParser.runParser(code))
    val scoutput = Scout.scoutProgram(parsed)
    val astrouts = Astronomer.runAstronomer(scoutput)
    val temputs = Templar.evaluate(astrouts)
    val hinputs = Carpenter.translate(temputs)
    val hamuts = Hammer.translate(hinputs)
    hamuts
  }

  def run(program: ProgramH, verbose: Boolean): IVonData = {
    if (verbose) {
      Vivem.executeWithPrimitiveArgs(
        program, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
    } else {
      Vivem.executeWithPrimitiveArgs(
        program,
        Vector(),
        new PrintStream(new OutputStream() {
          override def write(b: Int): Unit = {
            // System.out.write(b)
          }
        }),
        () => {
          scala.io.StdIn.readLine()
        },
        (str: String) => {
          print(str)
        })
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      vcheck(args.size >= 1, "Empty args!", InputException)
      args(0) match {
        case "build" => {
          vcheck(args.size >= 2, "Need name!", InputException)
          val code = readCode(args(1))
          val program = build(code)
          val programV = VonHammer.vonifyProgram(program)
          val json = new VonPrinter(JsonSyntax, 120).print(programV)
          println(json)
        }
        case "run" => {
          vcheck(args.size >= 2, "Need name!", InputException)
          val code = readCode(args(1))
          val program = build(code)

          val verbose = args.slice(2, args.length).contains("--verbose")
          val result =
            if (verbose) {
              Vivem.executeWithPrimitiveArgs(
                program, Vector(), System.out, Vivem.emptyStdin, Vivem.nullStdout)
            } else {
              val compile = new Compilation(code)

              Vivem.executeWithPrimitiveArgs(
                compile.getHamuts(),
                Vector(),
                new PrintStream(new OutputStream() {
                  override def write(b: Int): Unit = {
                    // System.out.write(b)
                  }
                }),
                () => {
                  scala.io.StdIn.readLine()
                },
                (str: String) => {
                  print(str)
                })
            }
          println("Program result: " + result)
          println()
          val programV = VonHammer.vonifyProgram(program)
          val json = new VonPrinter(JsonSyntax, 120).print(programV)
          println(json)
        }
      }
    } catch {
      case InputException(msg) => {
        println(msg)
        return
      }
    }
  }
}