package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.Scout.noDeclarations
import net.verdagon.vale.scout.patterns.PatternScout
import net.verdagon.vale.scout.patterns.PatternScout.{InitialRulesAndRunes, RuleState, RuleStateBox}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator

case class StackFrame(
    maybeParent: Option[StackFrame],
    // So that when we run into a magic param, we can add this to the number of previous magic
    // params to get the final param index.
    numExplicitParams: Int,
    locals: VariableDeclarations) {
  def ++(newVars: VariableDeclarations): StackFrame = {
    StackFrame(maybeParent, numExplicitParams, locals ++ newVars)
  }
  def allDeclarations: VariableDeclarations = {
    locals ++ maybeParent.map(_.allDeclarations).getOrElse(noDeclarations)
  }
  def getJumps(name: String): Option[Int] = {
    if (locals.contains(name)) {
      Some(0)
    } else {
      maybeParent match {
        case None => None
        case Some(parent) => {
          parent.getJumps(name) match {
            case None => None
            case Some(num) => Some(num + 1)
          }
        }
      }
    }
  }
}

object Scout {
  def noVariableUses = VariableUses(Set())
  def noDeclarations = VariableDeclarations(Set())

  val unnamedParamNamePrefix = "__param_"
  val unrunedParamRunePrefix = "__ParamRune_"
  val unrunedParamOverrideRuneSuffix = "Override"
  val unnamedMemberNameSeparator = "_mem_"
  val memberRuneSeparator = "_Mem_"
  val memberRunePrefix = "Mem_"

  def scoutProgram(program: Program0): ProgramS = {
    val Program0(structsP, interfacesP, implsP, functions0) = program;
    val structsS = scoutStructs(structsP);
    val interfacesS = scoutInterfaces(interfacesP);
    val implsS = scoutImpls(implsP);
    ProgramS(
      structsS,
      interfacesS,
      implsS,
      functions0.map(FunctionScout.scoutTopLevelFunction))
  }

  private def scoutImpl(impl0: ImplP): ImplS = {
    val ImplP(templateRulesP, struct, interface) = impl0
    val codeLocation = CodeLocationS("userInput.vale", impl0.pos.line, impl0.pos.column)

    val initialRulesAndRunes =
      InitialRulesAndRunes(List(), templateRulesP, List(), None)

    val rate = RuleStateBox(RuleState(RuleScout.translateRulexes(templateRulesP)))

    val structRune =
      PatternScout.translateMaybeTypeIntoRune(
        initialRulesAndRunes,
        rate,
        Some(struct),
        KindTypePR,
        None)

    val interfaceRune =
      PatternScout.translateMaybeTypeIntoRune(
        initialRulesAndRunes,
        rate,
        Some(interface),
        KindTypePR,
        None)

    val allRunes = PredictorEvaluator.getAllRunes(List(), rate.rate.rulexesS, List(), None)
    val Conclusions(knowableValueRunes, _) =
      PredictorEvaluator.solve(rate.rate.rulexesS, List())
    val isTemplate = knowableValueRunes != allRunes

    ImplS(codeLocation, rate.rate.rulexesS, allRunes, isTemplate, structRune, interfaceRune)
  }

  private def scoutStructs(structsP: List[StructP]): List[StructS] = {
    structsP.map({ case head @ StructP(structName, mutability, maybeIdentifyingRunes, rulesP, members) =>
      val codeLocation = CodeLocationS("userInput.vale", head.pos.line, head.pos.column)

      println("put identifying runes from params' top levels in!")

      val memberRunes = members.indices.map(memberRunePrefix + _)

      val rulesS =
        RuleScout.translateRulexes(rulesP) ++
          memberRunes.zip(members.map(_.tyype)).map({ case (memberRune, memberType) =>
            EqualsSR(TypedSR(Some(memberRune), CoordTypeSR), TemplexSR(TemplexScout.translateTemplex(memberType)))
          })

      val membersS =
        members.zip(memberRunes).map({ case (StructMemberP(name, variability, _), memberRune) =>
          StructMemberS(name, variability, memberRune)
        })

      val identifyingRunes = maybeIdentifyingRunes.getOrElse(List())

      val allRunes = PredictorEvaluator.getAllRunes(identifyingRunes, rulesS, List(), None)
      val Conclusions(knowableValueRunes, predictedTypeByRune) =
        PredictorEvaluator.solve(rulesS, List())
      val isTemplate = knowableValueRunes != allRunes

      val maybePredictedType =
        if (isTemplate) {
          if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
            Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), KindTypeSR))
          } else {
            None
          }
        } else {
          Some(KindTypeSR)
        }

      val maybePredictedMutability = Some(mutability)

      StructS(
        codeLocation,
        List(),
        structName,
        mutability,
        maybePredictedMutability,
        identifyingRunes,
        allRunes,
        maybePredictedType,
        isTemplate,
        rulesS,
        membersS)
    })
  }

  private def scoutInterfaces(interfacesP: List[InterfaceP]): List[InterfaceS] = {
    interfacesP.map({ case headP @ InterfaceP(name, mutability, maybeIdentifyingRunes, rulesP, internalMethodsP) =>
      val codeLocation = CodeLocationS("userInput.vale", headP.pos.line, headP.pos.column)

      val identifyingRunes = maybeIdentifyingRunes.getOrElse(List())

      val rulesS = RuleScout.translateRulexes(rulesP)

      val allRunes = PredictorEvaluator.getAllRunes(identifyingRunes, rulesS, List(), None)
      val Conclusions(knowableValueRunes, predictedTypeByRune) =
        PredictorEvaluator.solve(rulesS, List())
      val isTemplate = knowableValueRunes != allRunes

      val maybePredictedType =
        if (isTemplate) {
          if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
            Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), KindTypeSR))
          } else {
            None
          }
        } else {
          Some(KindTypeSR)
        }

      val maybePredictedMutability = Some(mutability)

      val internalMethodsS = internalMethodsP.map(FunctionScout.scoutInterfaceMember)

      val interfaceS =
        InterfaceS(
          codeLocation,
          List(),
          name,
          mutability,
          maybePredictedMutability,
          identifyingRunes,
          allRunes,
          maybePredictedType,
          isTemplate,
          rulesS,
          internalMethodsS)

      interfaceS
    })
  }


  private def scoutImpls(implsP: List[ImplP]): List[ImplS] = {
    implsP.map(scoutImpl)
  }


  def runScout(code: String): Option[ProgramS] = {
    VParser.runParser(code) match {
      case None => None
      case Some(program0) => {
        Some(runScout(program0))
      }
    }
  }

  def runScout(program0: Program0): ProgramS = {
    val ProgramS(originalStructs, originalInterfaces, originalImpls, originalImplementedFunctionsS) =
      Scout.scoutProgram(program0)
    val program1 =
      ProgramS(
        originalStructs,
        originalInterfaces,
        originalImpls,
        originalImplementedFunctionsS)
    program1
  }

}
