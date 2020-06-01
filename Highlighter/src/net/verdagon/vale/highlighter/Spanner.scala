package net.verdagon.vale.highlighter

import net.verdagon.vale.parser._
import net.verdagon.vale.vimpl

sealed trait IClass
case object Prog extends IClass
case object W extends IClass
case object Abst extends IClass
case object Ext extends IClass
case object Fn extends IClass
case object FnName extends IClass
case object Rules extends IClass
case object Rune extends IClass
case object IdentRunes extends IClass
case object IdentRune extends IClass
case object Params extends IClass
case object Pat extends IClass
case object Block extends IClass
case object Num extends IClass

case class Span(classs: IClass, range: Range, children: List[Span] = List())

object Spanner {
  def forProgram(program: Program0): Span = {
    Span(
      Prog,
      Range(Pos(1, 1), Pos(Int.MaxValue, Int.MaxValue)),
      program.topLevelThings.flatMap({
        case TopLevelFunction(f) => List(forFunction(f))
      }))
  }

  def forFunction(function: FunctionP): Span = {
    val FunctionP(range, Some(name), isExtern, isAbstract, maybeUserSpecifiedIdentifyingRunes, templateRules, params, ret, body) = function

    Span(
      Fn,
      range,
      isExtern.toList.map(e => Span(Ext, e.range)) ++
      isAbstract.toList.map(e => Span(Abst, e.range)) ++
      List(Span(FnName, name.range)) ++
      maybeUserSpecifiedIdentifyingRunes.toList.map(forIdentifyingRunes) ++
      templateRules.toList.map(forTemplateRules) ++
      params.toList.map(forParams) ++
      ret.toList.map(forTemplexPP) ++
      body.toList.map(forBlock))
  }

  def forBlock(b: BlockPE): Span = {
    val BlockPE(range, elements) = b
    Span(Block, range, elements.map(forExpression))
  }

  def forExpression(e: IExpressionPE): Span = {
    e match {
      case IntLiteralPE(range, _) => Span(Num, range, List())
      case other => vimpl(other.toString)
    }
  }

  def forParams(p: ParamsP): Span = {
    val ParamsP(range, params) = p
    Span(Params, range, params.map(forPattern))
  }

  def forPattern(p: PatternPP): Span = {
    val PatternPP(range, capture, templex, destructure, virtuality) = p
    Span(
      Pat,
      range,
      List())
  }

  def forTemplexPP(t: ITemplexPPT): Span = {
    vimpl()
  }

  def forTemplateRules(rulesP: TemplateRulesP): Span = {
    val TemplateRulesP(range, rules) = rulesP
    Span(
      Rules,
      range,
      rules.map(forRulex))
  }

  def forRulex(rulex: IRulexPR): Span = {
    vimpl()
  }

  def forIdentifyingRunes(r: IdentifyingRunesP): Span = {
    val IdentifyingRunesP(range, runes) = r
    Span(
      IdentRunes,
      range,
      runes.map(rune => Span(IdentRune, rune.range)))
  }
}
