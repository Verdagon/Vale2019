package net.verdagon.vale.driver

import java.io.{BufferedWriter, File, FileWriter, OutputStream, PrintStream}
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

  val usage = """
    Usage: mmlaln [--min-size num] [--max-size num] filename
  """

  def main(args: Array[String]): Unit = {
    try {
      case class Options(
        inputFiles: List[String],
        outputFile: Option[String],
        mode: Option[String], // build v run etc
      )

      def parseOpts(opts: Options, list: List[String]) : Options = {
        list match {
          case Nil => opts
          case "-o" :: value :: tail => {
            vcheck(opts.outputFile.isEmpty, "Multiple output files specified!", InputException)
            parseOpts(opts.copy(outputFile = Some(value)), tail)
          }
//          case "--min-size" :: value :: tail =>
//            parseOpts(opts ++ Map('minsize -> value.toInt), tail)
//          case string :: opt2 :: tail if isSwitch(opt2) =>
//            parseOpts(opts ++ Map('infile -> string), list.tail)
          case value :: _ if value.startsWith("-") => throw InputException("Unknown option " + value)
          case value :: tail => {
            if (opts.mode.isEmpty) {
              parseOpts(opts.copy(mode = Some(value)), tail)
            } else {
              parseOpts(opts.copy(inputFiles = opts.inputFiles :+ value), tail)
            }
          }
        }
      }
      val opts = parseOpts(Options(List(), None, None), args.toList)
      vcheck(opts.mode.nonEmpty, "No mode!", InputException)
      vcheck(opts.inputFiles.nonEmpty, "No input files!", InputException)
      vcheck(opts.outputFile.nonEmpty, "No output file!", InputException)

      opts.mode.get match {
        case "build" => {
          val code = opts.inputFiles.map(readCode).mkString("\n\n\n")
          val program = build(code)
          val programV = VonHammer.vonifyProgram(program)
          val json = new VonPrinter(JsonSyntax, 120).print(programV)
          println("Wrote to file " + opts.outputFile.get)
          writeFile(opts.outputFile.get, json)
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
          println("Wrote to file " + opts.outputFile.get)
          writeFile(opts.outputFile.get, json)
        }
      }
    } catch {
      case InputException(msg) => {
        println(msg)
        return
      }
    }
  }

  def writeFile(filename: String, s: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s)
    bw.close()
  }
}