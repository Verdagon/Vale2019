package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.vimpl

object NameTranslator {
//  def translateImpreciseTypeName(fullNameA: ImpreciseNameA[CodeTypeNameA]): ImpreciseName2[CodeTypeName2] = {
//    val ImpreciseNameA(initS, lastS) = fullNameA
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateCodeTypeName(lastS))
//  }
//
//  def translateImpreciseName(fullNameA: ImpreciseNameA[IImpreciseNameStepA]): ImpreciseName2[IImpreciseNameStep2] = {
//    val ImpreciseNameA(initS, lastS) = fullNameA
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateImpreciseNameStep(lastS))
//  }
//
//  def translateCodeTypeName(codeTypeNameA: CodeTypeNameA): CodeTypeName2 = {
//    val CodeTypeNameA(name) = codeTypeNameA
//    CodeTypeName2(name)
//  }
//
//  def translateImpreciseNameStep(impreciseNameStepA: IImpreciseNameStepA): IImpreciseNameStep2 = {
//    impreciseNameStepA match {
//      case ctn @ CodeTypeNameA(_) => translateCodeTypeName(ctn)
//      case GlobalFunctionFamilyNameA(name) => GlobalFunctionFamilyName2(name)
//      case icvn @ ImpreciseCodeVarNameA(_) => translateImpreciseCodeVarNameStep(icvn)
//    }
//  }
//
//  def translateImpreciseCodeVarNameStep(impreciseNameStepA: ImpreciseCodeVarNameA): ImpreciseCodeVarName2 = {
//    var ImpreciseCodeVarNameA(name) = impreciseNameStepA
//    ImpreciseCodeVarName2(name)
//  }

  def translateRuneAbsoluteName(absoluteNameA: AbsoluteNameA[IRuneA]): FullName2[IRune2] = {
    val AbsoluteNameA(file, initS, lastS) = absoluteNameA
    FullName2(file, initS.map(translateNameStep), translateRune(lastS))
  }

  def translateVarAbsoluteName(absoluteNameA: AbsoluteNameA[IVarNameA]): FullName2[IVarName2] = {
    val AbsoluteNameA(file, initS, lastS) = absoluteNameA
    FullName2(file, initS.map(translateNameStep), translateVarNameStep(lastS))
  }
//
//  def translateVarImpreciseName(absoluteNameA: ImpreciseNameA[ImpreciseCodeVarNameA]):
//  ImpreciseName2[ImpreciseCodeVarName2] = {
//    val ImpreciseNameA(initS, lastS) = absoluteNameA
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateImpreciseCodeVarNameStep(lastS))
//  }
//
//  def translateFunctionFamilyName(name: ImpreciseNameA[GlobalFunctionFamilyNameA]):
//  ImpreciseName2[GlobalFunctionFamilyName2] = {
//    val ImpreciseNameA(init, last) = name
//    ImpreciseName2(init.map(translateImpreciseNameStep), translateGlobalFunctionFamilyName(last))
//  }
//
//  def translateGlobalFunctionFamilyName(s: GlobalFunctionFamilyNameA): GlobalFunctionFamilyName2 = {
//    val GlobalFunctionFamilyNameA(name) = s
//    GlobalFunctionFamilyName2(name)
//  }

  def translateAbsoluteName(absoluteNameA: AbsoluteNameA[INameA]): FullName2[IName2] = {
    val AbsoluteNameA(file, initS, lastS) = absoluteNameA
    FullName2(file, initS.map(translateNameStep), translateNameStep(lastS))
  }

  def translateNameStep(name: INameA): IName2 = {
    name match {
//      case LambdaNameA(codeLocation) => LambdaName2(codeLocation)
//      case FunctionNameA(name, codeLocation) => FunctionName2(name, codeLocation)
//      case TopLevelCitizenDeclarationNameA(name, codeLocation) => TopLevelCitizenDeclarationName2(name, codeLocation)
//      case LambdaStructNameA(codeLocation) => LambdaStructName2(codeLocation)
      case ImplNameA(codeLocation) => ImplName2(codeLocation)
      case LetNameA(codeLocation) => LetName2(codeLocation)
      case UnnamedLocalNameA(codeLocation) => UnnamedLocalName2(codeLocation)
      case ClosureParamNameA() => ClosureParamName2()
      case MagicParamNameA(magicParamNumber) => MagicParamName2(magicParamNumber)
      case CodeVarNameA(name) => CodeVarName2(name)
      case CodeRuneA(name) => CodeRune2(name)
      case ImplicitRuneA(name) => ImplicitRune2(name)
      case MagicImplicitRuneA(magicParamIndex) => MagicImplicitRune2(magicParamIndex)
      case MemberRuneA(memberIndex) => MemberRune2(memberIndex)
      case ReturnRuneA() => ReturnRune2()
      case _ => vimpl()
    }
  }

  def translateVarNameStep(name: IVarNameA): IVarName2 = {
    name match {
      case UnnamedLocalNameA(codeLocation) => UnnamedLocalName2(codeLocation)
      case ClosureParamNameA() => ClosureParamName2()
      case MagicParamNameA(magicParamNumber) => MagicParamName2(magicParamNumber)
      case CodeVarNameA(name) => CodeVarName2(name)
    }
  }

  def translateRune(rune: IRuneA): IRune2 = {
    rune match {
      case CodeRuneA(name) => CodeRune2(name)
      case ImplicitRuneA(name) => ImplicitRune2(name)
      case MagicImplicitRuneA(magicParamIndex) => MagicImplicitRune2(magicParamIndex)
      case MemberRuneA(memberIndex) => MemberRune2(memberIndex)
      case ReturnRuneA() => ReturnRune2()
    }
  }
}