package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{PatternScout, RuleState, RuleStateBox}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale.vimpl

sealed trait IEnvironment {
  def name: AbsoluteNameS[INameS]
  def allUserDeclaredRunes(): Set[AbsoluteNameS[IRuneS]]
}

// Someday we might split this into NamespaceEnvironment and CitizenEnvironment
case class Environment(
    parentEnv: Option[Environment],
    name: AbsoluteNameS[INameS],
    userDeclaredRunes: Set[AbsoluteNameS[IRuneS]]
) extends IEnvironment {
  override def allUserDeclaredRunes(): Set[AbsoluteNameS[IRuneS]] = {
    userDeclaredRunes ++ parentEnv.toList.flatMap(pe => pe.allUserDeclaredRunes())
  }
}

case class FunctionEnvironment(
    name: AbsoluteNameS[IFunctionDeclarationNameS],
    parentEnv: Option[IEnvironment],
    userDeclaredRunes: Set[AbsoluteNameS[IRuneS]],
    // So that when we run into a magic param, we can add this to the number of previous magic
    // params to get the final param index.
    numExplicitParams: Int
) extends IEnvironment {
  override def allUserDeclaredRunes(): Set[AbsoluteNameS[IRuneS]] = {
    userDeclaredRunes ++ parentEnv.toList.flatMap(_.allUserDeclaredRunes())
  }
}

case class StackFrame(
    name: AbsoluteNameS[IFunctionDeclarationNameS],
    parentEnv: FunctionEnvironment,
    maybeParent: Option[StackFrame],
    locals: VariableDeclarations) {
  def ++(newVars: VariableDeclarations): StackFrame = {
    StackFrame(name, parentEnv, maybeParent, locals ++ newVars)
  }
  def allDeclarations: VariableDeclarations = {
    locals ++ maybeParent.map(_.allDeclarations).getOrElse(Scout.noDeclarations)
  }
  def findVariable(name: String): Option[AbsoluteNameS[IVarNameS]] = {
    locals.find(name) match {
      case Some(fullNameS) => Some(fullNameS)
      case None => {
        maybeParent match {
          case None => None
          case Some(parent) => parent.findVariable(name)
        }
      }
    }
  }
}

object Scout {
  def noVariableUses = VariableUses(Set())
  def noDeclarations = VariableDeclarations(Set())

//  val unnamedParamNamePrefix = "__param_"
//  val unrunedParamOverrideRuneSuffix = "Override"
//  val unnamedMemberNameSeparator = "_mem_"

  def scoutProgram(program: Program0): ProgramS = {
    val Program0(structsP, interfacesP, implsP, functions0) = program;
    val file = "in.vale"
    val structsS = structsP.map(scoutStruct(file, _));
    val interfacesS = interfacesP.map(scoutInterface(file, _));
    val implsS = implsP.map(scoutImpl(file, _))
    val functionsS = functions0.map(FunctionScout.scoutTopLevelFunction("in.vale", _))
    ProgramS(structsS, interfacesS, implsS, functionsS)
  }

  private def scoutImpl(file: String, impl0: ImplP): ImplS = {
    val ImplP(identifyingRuneNames, templateRulesP, struct, interface) = impl0

    val codeLocation = CodeLocationS(impl0.pos.line, impl0.pos.column)
    val nameS = AbsoluteNameS(file, List(), ImplNameS(codeLocation))

    val identifyingRunes: List[AbsoluteNameS[IRuneS]] =
      identifyingRuneNames
        .map(identifyingRuneName => nameS.addStep(CodeRuneS(identifyingRuneName)))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map(identifyingRuneName => nameS.addStep(CodeRuneS(identifyingRuneName)))
    val userDeclaredRunes = identifyingRunes ++ runesFromRules

    val implEnv = Environment(None, nameS, userDeclaredRunes.toSet)

    val rate = RuleStateBox(RuleState(implEnv.name, 0))
    val userRulesS =
      RuleScout.translateRulexes(rate, implEnv.allUserDeclaredRunes(), templateRulesP)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(implEnv.name, identifyingRunes, userRulesS, List(), None)
    val Conclusions(knowableValueRunes, _) = PredictorEvaluator.solve(userRulesS, List())
    val isTemplate = knowableValueRunes != allRunes

    val (implicitRulesFromStruct, structRune) =
      PatternScout.translateMaybeTypeIntoRune(
        userDeclaredRunes.toSet,
        rate,
        Some(struct),
        KindTypePR)

    val (implicitRulesFromInterface, interfaceRune) =
      PatternScout.translateMaybeTypeIntoRune(
        userDeclaredRunes.toSet,
        rate,
        Some(interface),
        KindTypePR)

    val rulesS = userRulesS ++ implicitRulesFromStruct ++ implicitRulesFromInterface

    ImplS(nameS, rulesS, allRunes, isTemplate, structRune, interfaceRune)
  }

  private def scoutStruct(file: String, head: StructP): StructS = {
    val StructP(structHumanName, mutability, maybeIdentifyingRunes, rulesP, members) = head
    val codeLocation = CodeLocationS(head.pos.line, head.pos.column)
    val structName = AbsoluteNameS(file, List(), TopLevelCitizenDeclarationNameS(structHumanName, codeLocation))

    val identifyingRunes: List[AbsoluteNameS[IRuneS]] =
      maybeIdentifyingRunes.getOrElse(List())
        .map(identifyingRuneName => structName.addStep(CodeRuneS(identifyingRuneName)))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(head.templateRules)
        .map(identifyingRuneName => structName.addStep(CodeRuneS(identifyingRuneName)))
    val userDeclaredRunes = identifyingRunes ++ runesFromRules
    val structEnv = Environment(None, structName, userDeclaredRunes.toSet)

    val memberRunes = members.indices.map(index => structName.addStep(MemberRuneS(index)))
    val memberRules =
      memberRunes.zip(members.map(_.tyype)).map({ case (memberRune, memberType) =>
        EqualsSR(
          TypedSR(memberRune, CoordTypeSR),
          TemplexSR(TemplexScout.translateTemplex(structEnv.allUserDeclaredRunes(), memberType)))
      })

    val rate = RuleStateBox(RuleState(structEnv.name, 0))
    val rulesS =
      RuleScout.translateRulexes(rate, structEnv.allUserDeclaredRunes(), rulesP) ++
      memberRules

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(structEnv.name, identifyingRunes, rulesS, List(), None)
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

  private def scoutInterface(file: String, headP: InterfaceP): InterfaceS = {
    val InterfaceP(interfaceHumanName, mutability, maybeIdentifyingRunes, rulesP, internalMethodsP) = headP
    val codeLocation = CodeLocationS(headP.pos.line, headP.pos.column)
    val interfaceFullName = AbsoluteNameS(file, List(), TopLevelCitizenDeclarationNameS(interfaceHumanName, codeLocation))

    val identifyingRunes: List[AbsoluteNameS[IRuneS]] =
      maybeIdentifyingRunes.getOrElse(List())
        .map(identifyingRuneName => interfaceFullName.addStep(CodeRuneS(identifyingRuneName)))
    val runesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(rulesP)
        .map(identifyingRuneName => interfaceFullName.addStep(CodeRuneS(identifyingRuneName)))
    val userDeclaredRunes = identifyingRunes ++ runesFromRules
    val interfaceEnv = Environment(None, interfaceFullName, userDeclaredRunes.toSet)

    val ruleState = RuleStateBox(RuleState(interfaceEnv.name, 0))
    val rulesS = RuleScout.translateRulexes(ruleState, interfaceEnv.allUserDeclaredRunes(), rulesP)

    // We gather all the runes from the scouted rules to be consistent with the function scout.
    val allRunes = PredictorEvaluator.getAllRunes(interfaceFullName, identifyingRunes, rulesS, List(), None)
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
        interfaceFullName,
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
}
