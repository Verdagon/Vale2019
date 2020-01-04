package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.PatternScout
import net.verdagon.vale.scout.patterns.PatternScout.{InitialRulesAndRunes, RuleState, RuleStateBox}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale.vimpl

sealed trait IEnvironment {
  def allUserDeclaredRunes(): Set[String]
  def file: String
}

// Someday we might split this into NamespaceEnvironment and CitizenEnvironment
case class Environment(
    file: String,
    parentEnv: Option[Environment],
    userDeclaredRunes: Set[String]
) extends IEnvironment {
  override def allUserDeclaredRunes(): Set[String] = {
    userDeclaredRunes ++ parentEnv.toList.flatMap(pe => pe.allUserDeclaredRunes())
  }
}

case class FunctionEnvironment(
    parentEnv: IEnvironment,
    userDeclaredRunes: Set[String],
    // So that when we run into a magic param, we can add this to the number of previous magic
    // params to get the final param index.
    numExplicitParams: Int
) extends IEnvironment {
  def file: String = parentEnv.file
  override def allUserDeclaredRunes(): Set[String] = {
    userDeclaredRunes ++ parentEnv.allUserDeclaredRunes()
  }
}

case class StackFrame(
    parentEnv: FunctionEnvironment,
    maybeParent: Option[StackFrame],
    locals: VariableDeclarations) {
  def ++(newVars: VariableDeclarations): StackFrame = {
    StackFrame(parentEnv, maybeParent, locals ++ newVars)
  }
  def allDeclarations: VariableDeclarations = {
    locals ++ maybeParent.map(_.allDeclarations).getOrElse(Scout.noDeclarations)
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

  def scoutProgram(program: Program0): ProgramS = {
    val Program0(structsP, interfacesP, implsP, functions0) = program;
    val rootEnv = Environment("userInput.vale", None, Set())
    val structsS = structsP.map(scoutStruct(rootEnv, _));
    val interfacesS = interfacesP.map(scoutInterface(rootEnv, _));
    val implsS = implsP.map(scoutImpl(rootEnv, _))
    val functionsS = functions0.map(FunctionScout.scoutTopLevelFunction(rootEnv, _))
    ProgramS(structsS, interfacesS, implsS, functionsS)
  }

  private def scoutImpl(rootEnv: Environment, impl0: ImplP): ImplS = {
    val ImplP(templateRulesP, struct, interface) = impl0

    val userDeclaredRunes =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
    val implEnv = Environment(rootEnv.file, Some(rootEnv), userDeclaredRunes.toSet)

    val codeLocation = CodeLocationS("userInput.vale", impl0.pos.line, impl0.pos.column)

    val initialRulesAndRunes = InitialRulesAndRunes(List(), templateRulesP, List(), None)

    val rate = RuleStateBox(RuleState(RuleScout.translateRulexes(implEnv.allUserDeclaredRunes(), templateRulesP)))

    val declaredRunes = vimpl()

    val structRune =
      PatternScout.translateMaybeTypeIntoRune(
        declaredRunes,
        initialRulesAndRunes,
        rate,
        Some(struct),
        KindTypePR,
        None)

    val interfaceRune =
      PatternScout.translateMaybeTypeIntoRune(
        declaredRunes,
        initialRulesAndRunes,
        rate,
        Some(interface),
        KindTypePR,
        None)

    val rulesS = rate.rate.rulexesS
    val allRunes = PredictorEvaluator.getAllRunes(List(), rulesS, List(), None)
    val Conclusions(knowableValueRunes, _) =
      PredictorEvaluator.solve(rate.rate.rulexesS, List())
    val isTemplate = knowableValueRunes != allRunes

    ImplS(codeLocation, rulesS, allRunes, isTemplate, structRune, interfaceRune)
  }

  private def scoutStruct(rootEnv: Environment, head: StructP): StructS = {
    val StructP(structName, mutability, maybeIdentifyingRunes, rulesP, members) = head
    val codeLocation = CodeLocationS(rootEnv.file, head.pos.line, head.pos.column)

    val identifyingRunes = maybeIdentifyingRunes.getOrElse(List())
    val userDeclaredRunes =
      identifyingRunes ++
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(head.templateRules)
    val structEnv = Environment(rootEnv.file, Some(rootEnv), userDeclaredRunes.toSet)

    val memberRunes = members.indices.map(Scout.makeMemberRune(codeLocation, _))
    val memberRules =
      memberRunes.zip(members.map(_.tyype)).map({ case (memberRune, memberType) =>
        EqualsSR(
          TypedSR(Some(memberRune), CoordTypeSR),
          TemplexSR(TemplexScout.translateTemplex(structEnv.allUserDeclaredRunes(), memberType)))
      })
    val rulesS =
      RuleScout.translateRulexes(structEnv.allUserDeclaredRunes(), rulesP) ++
      memberRules

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(identifyingRunes, rulesS, List(), None)
    val Conclusions(knowableValueRunes, predictedTypeByRune) = PredictorEvaluator.solve(rulesS, List())
    val isTemplate = knowableValueRunes != allRunes

    val membersS =
      members.zip(memberRunes).map({ case (StructMemberP(name, variability, _), memberRune) =>
        StructMemberS(name, variability, memberRune)
      })

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
      allRunes.toSet,
      maybePredictedType,
      isTemplate,
      rulesS,
      membersS)
  }

  private def scoutInterface(rootEnv: Environment, headP: InterfaceP): InterfaceS = {
    val InterfaceP(name, mutability, maybeIdentifyingRunes, rulesP, internalMethodsP) = headP
    val codeLocation = CodeLocationS(rootEnv.file, headP.pos.line, headP.pos.column)

    val identifyingRunes = maybeIdentifyingRunes.getOrElse(List())
    val userDeclaredRunes =
      identifyingRunes ++
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(headP.templateRules)
    val interfaceEnv = Environment(rootEnv.file, Some(rootEnv), userDeclaredRunes.toSet)

    val rulesS = RuleScout.translateRulexes(interfaceEnv.allUserDeclaredRunes(), rulesP)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(identifyingRunes, rulesS, List(), None)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(rulesS, List())
    val isTemplate = knowableValueRunes != userDeclaredRunes.toSet

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

    val internalMethodsS = internalMethodsP.map(FunctionScout.scoutInterfaceMember(interfaceEnv, _))

    val interfaceS =
      InterfaceS(
        codeLocation,
        List(),
        name,
        mutability,
        maybePredictedMutability,
        identifyingRunes,
        allRunes.toSet,
        maybePredictedType,
        isTemplate,
        rulesS,
        internalMethodsS)

    interfaceS
  }

  def runScout(code: String): Option[ProgramS] = {
    VParser.runParser(code) match {
      case None => None
      case Some(program0) => {
        Some(scoutProgram(program0))
      }
    }
  }

  def makeMemberRune(structCodeLocation: CodeLocationS, index: Int): String = {
    structCodeLocation.file + ":" + structCodeLocation.line + ":" + structCodeLocation.char + memberRuneSeparator + index
  }
}
