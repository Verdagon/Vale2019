package net.verdagon.radonc.scout

import net.verdagon.radonc.scout.rules.ITypeSR

package object predictor {
  case class PredictorEvaluateResult[T](
    conclusions: Conclusions,
    result: T)

  case class Conclusions(
      knowableValueRunes: Set[String],
      predictedTypeByRune: Map[String, ITypeSR]) {
    def markRuneValueKnowable(rune: String): Conclusions = {
      Conclusions(
        knowableValueRunes + rune,
        predictedTypeByRune)
    }
    def markRuneTypeKnown(rune: String, tyype: ITypeSR): Conclusions = {
      Conclusions(
        knowableValueRunes,
        predictedTypeByRune + (rune -> tyype))
    }
  }
}
