package net.verdagon.vale

import net.verdagon.vale.astronomer.{Astronomer, ProgramA}
import net.verdagon.vale.carpenter.Carpenter
import net.verdagon.vale.hammer.{Hammer, Program3}
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.parser.{Program0, VParser}
import net.verdagon.vale.scout.{ProgramS, Scout}
import net.verdagon.vale.templar.{CommonEnv, CompleteProgram2, Program2, Templar}
import net.verdagon.vale.vivem.{Heap, PrimitiveReferendV, ReferenceV, Vivem}
import net.verdagon.von.IVonData

class Compilation(code: String, useCommonEnv: Boolean = true) {
  var parsedCache: Option[Program0] = None
  var scoutputCache: Option[ProgramS] = None
  var astroutsCache: Option[ProgramA] = None
  var temputsCache: Option[CompleteProgram2] = None
  var hinputsCache: Option[Hinputs] = None
  var hamutsCache: Option[Program3] = None

  def getParsed(): Program0 = {
    parsedCache match {
      case Some(parsed) => parsed
      case None => {
        val parsed = VParser.runParser(code)
        vassert(parsed != None) // runNamifier returns a None if it failed
        parsedCache = parsed
        parsed.get
      }
    }
  }

  def getScoutput(): ProgramS = {
    scoutputCache match {
      case Some(scoutput) => scoutput
      case None => {
        val scoutput =
          if (useCommonEnv) {
            Scout.runScout(getParsed())
          } else {
            Scout.scoutProgram(getParsed())
          }
        scoutputCache = Some(scoutput)
        scoutput
      }
    }
  }

  def getAstrouts(): ProgramA = {
    astroutsCache match {
      case Some(astrouts) => astrouts
      case None => {
        val astrouts = Astronomer.runAstronomer(getScoutput())
        astroutsCache = Some(astrouts)
        astrouts
      }
    }
  }

  def getTemputs(): CompleteProgram2 = {
    temputsCache match {
      case Some(temputs) => temputs
      case None => {
        val temputs =
          if (useCommonEnv) {
            Templar.runTemplar(getAstrouts())
          } else {
            Templar.evaluate(getAstrouts())
          }
        temputsCache = Some(temputs)
        temputs
      }
    }
  }

  def getHinputs(): Hinputs = {
    hinputsCache match {
      case Some(hinputs) => hinputs
      case None => {
        val hinputs = Carpenter.translate(getTemputs())
        hinputsCache = Some(hinputs)
        hinputs
      }
    }
  }

  def getHamuts(): Program3 = {
    hamutsCache match {
      case Some(hamuts) => hamuts
      case None => {
        val hamuts = Hammer.translate(getHinputs())
        hamutsCache = Some(hamuts)
        hamuts
      }
    }
  }

  def evalForReferend(heap: Heap, args: Vector[ReferenceV]): IVonData = {
    Vivem.executeWithHeap(getHamuts(), heap, args, System.out, Vivem.emptyStdin, Vivem.regularStdout).get
  }
  def run(heap: Heap, args: Vector[ReferenceV]): Unit = {
    Vivem.executeWithHeap(getHamuts(), heap, args, System.out, Vivem.emptyStdin, Vivem.regularStdout)
  }
  def run(args: Vector[PrimitiveReferendV]): Unit = {
    Vivem.executeWithPrimitiveArgs(getHamuts(), args, System.out, Vivem.emptyStdin, Vivem.regularStdout)
  }
  def evalForReferend(args: Vector[PrimitiveReferendV]): IVonData = {
    Vivem.executeWithPrimitiveArgs(getHamuts(), args, System.out, Vivem.emptyStdin, Vivem.regularStdout).get
  }
  def evalForReferend(
      args: Vector[PrimitiveReferendV],
      stdin: List[String]):
  IVonData = {
    Vivem.executeWithPrimitiveArgs(getHamuts(), args, System.out, Vivem.stdinFromList(stdin), Vivem.regularStdout).get
  }
  def evalForStdout(args: Vector[PrimitiveReferendV]): String = {
    val (stdoutStringBuilder, stdoutFunc) = Vivem.stdoutCollector()
    Vivem.executeWithPrimitiveArgs(getHamuts(), args, System.out, Vivem.emptyStdin, stdoutFunc)
    stdoutStringBuilder.mkString
  }
  def evalForReferendAndStdout(args: Vector[PrimitiveReferendV]): (IVonData, String) = {
    val (stdoutStringBuilder, stdoutFunc) = Vivem.stdoutCollector()
    val referend = Vivem.executeWithPrimitiveArgs(getHamuts(), args, System.out, Vivem.emptyStdin, stdoutFunc)
    (referend.get, stdoutStringBuilder.mkString)
  }
}