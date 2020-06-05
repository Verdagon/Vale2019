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
case object Capture extends IClass
case object CaptureName extends IClass
case object Block extends IClass
case object Num extends IClass
case object Str extends IClass
case object Bool extends IClass
case object Typ extends IClass
case object Call extends IClass
case object Ret extends IClass
case object CallLookup extends IClass
case object Lookup extends IClass
case object Seq extends IClass
case object Mut extends IClass
case object MemberAccess extends IClass
case object Let extends IClass
case object Lambda extends IClass
case object MagicParam extends IClass

case class Span(classs: IClass, range: Range, children: List[Span] = List())

object Spanner {
  def forProgram(program: Program0): Span = {
    Span(
      Prog,
      Range(Pos(1, 1), Pos(Int.MaxValue, Int.MaxValue)),
      program.topLevelThings.flatMap({
        case TopLevelFunction(f) => List(forFunction(f))
        case TopLevelInterface(_) => List()
        case TopLevelStruct(_) => List()
        case TopLevelImpl(_) => List()
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
      case StrLiteralPE(StringP(range, _)) => Span(Str, range, List())
      case BoolLiteralPE(range, _) => Span(Bool, range, List())
      case VoidPE(range) => Span(W, range, List())
      case MagicParamLookupPE(range) => {
        Span(
          MagicParam,
          range,
          List())
      }
      case LambdaPE(FunctionP(range, None, _, _, _, _, params, _, body)) => {
        Span(
          Lambda,
          range,
          params.toList.map(forParams) ++ body.toList.map(forBlock))
      }
      case LetPE(range, templateRules, pattern, expr) => {
        Span(
          Let,
          range,
          List(forPattern(pattern), forExpression(expr)))
      }
      case LookupPE(StringP(range, _), templateArgs) => {
        Span(Lookup, range, List())
      }
      case SequencePE(range, elements) => {
        Span(Seq, range, elements.map(forExpression))
      }
      case MutatePE(range, mutatee, expr) => {
        Span(Mut, range, List(forExpression(mutatee), forExpression(expr)))
      }
      case DotPE(range, left, member) => {
        Span(
          MemberAccess,
          range,
          List(forExpression(left), forExpression(member)))
      }
      case DotCallPE(range, callableExpr, argExprs) => {
        val callableSpan = forExpression(callableExpr)
        val argSpans = argExprs.map(forExpression)
        val allSpans = (callableSpan :: argSpans)
        Span(Call, range, allSpans)
      }
      case MethodCallPE(range, callableExpr, _, LookupPE(StringP(nameRange, _), _), argExprs) => {
        val callableSpan = forExpression(callableExpr)
        val methodSpan = Span(CallLookup, nameRange, List())
        val argSpans = argExprs.map(forExpression)
        val allSpans = (callableSpan :: methodSpan :: argSpans)
        Span(Call, range, allSpans)
      }
      case FunctionCallPE(range, LookupPE(StringP(nameRange, _), _), argExprs, _) => {
        val callableSpan = Span(CallLookup, nameRange, List())
        val argSpans = argExprs.map(forExpression)
        val allSpans = (callableSpan :: argSpans).sortWith(_.range.begin < _.range.begin)
        Span(Call, range, allSpans)
      }
      case FunctionCallPE(range, callableExpr, argExprs, _) => {
        val callableSpan = forExpression(callableExpr)
        val argSpans = argExprs.map(forExpression)
        val allSpans = (callableSpan :: argSpans).sortWith(_.range.begin < _.range.begin)
        Span(Call, range, allSpans)
      }
      case ReturnPE(range, expr) => {
        Span(Ret, range, List(forExpression(expr)))
      }
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
      capture.toList.map(forCapture) ++ templex.toList.map(forTemplexPP))
  }

  def forCapture(c: CaptureP): Span = {
    val CaptureP(range, StringP(nameRange, _), _) = c
    Span(
      Capture,
      range,
      List(Span(CaptureName, nameRange, List())))
  }

  def forTemplexPP(t: ITemplexPPT): Span = {
    t match {
      case NameOrRunePPT(StringP(range, name)) => {
        Span(Typ, range, List())
      }
      case _ => vimpl()
    }
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
