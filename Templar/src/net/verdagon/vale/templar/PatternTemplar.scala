package net.verdagon.vale.templar;

import net.verdagon.vale.astronomer.{IRulexAR, ITemplataType}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser.CaptureP
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules.IRulexSR
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.DestructorTemplar
import net.verdagon.vale.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.templar.templata.TemplataTemplar
import net.verdagon.vale.vfail

import scala.collection.immutable.List

// either want:
// 1. (nonchecking) thing that just trusts its good and extracts it into locals. (for lets)
// 2. (checking) thing that checks it matches, returns None if not, otherwise returns
//    a struct containing it all (for signatures)

object PatternTemplar {

  // Note: This will unlet/drop the input expressions. Be warned.
  // patternInputExprs2 is a list of reference expression because they're coming in from
  // god knows where... arguments, the right side of a let, a variable, don't know!
  // If a pattern needs to send it to multiple places, the pattern is free to put it into
  // a local variable.
  // PatternTemplar must be sure to NOT USE IT TWICE! That would mean copying the entire
  // expression subtree that it contains!
  // Has "InferAnd" because we evaluate the template rules too.
  // Returns:
  // - Temputs
  // - Function state
  // - Exports, to toss into the environment
  // - Local variables
  def nonCheckingInferAndTranslateList(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    patterns1: List[AtomSP],
    patternInputExprs2: List[ReferenceExpression2]):
  (List[ReferenceExpression2]) = {

    val patternInputCoords = patternInputExprs2.map(_.resultRegister.reference)

    val templatasByRune =
      InferTemplar.inferFromArgCoords(fate.snapshot, temputs, List(), rules, typeByRune, patterns1, None, List(), patternInputCoords.map(arg => ParamFilter(arg, None))) match {
        case (InferSolveFailure(_, _, _, _, _, _)) => vfail("Couldn't figure out runes for pattern!")
        case (InferSolveSuccess(tbr)) => (tbr.templatasByRune.mapValues(v => List(TemplataEnvEntry(v))))
      }

    fate.addEntries(templatasByRune)

    nonCheckingTranslateList(temputs, fate, patterns1, patternInputExprs2)
  }

  // Note: This will unlet/drop the input expressions. Be warned.
  // patternInputExprs2 is a list of reference expression because they're coming in from
  // god knows where... arguments, the right side of a let, a variable, don't know!
  // If a pattern needs to send it to multiple places, the pattern is free to put it into
  // a local variable.
  // PatternTemplar must be sure to NOT USE IT TWICE! That would mean copying the entire
  // expression subtree that it contains!
  // Has "InferAnd" because we evaluate the template rules too.
  // Returns:
  // - Temputs
  // - Function state
  // - Exports, to toss into the environment
  // - Local variables
  def nonCheckingTranslateList(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
      patterns1: List[AtomSP],
      patternInputExprs2: List[ReferenceExpression2]):
  (List[ReferenceExpression2]) = {

    patterns1.zip(patternInputExprs2) match {
      case Nil => (Nil)
      case (pattern1, patternInputExpr2) :: _ => {
        val (headLets) =
          PatternTemplar.innerNonCheckingTranslate(
            temputs, fate, pattern1, patternInputExpr2);
        val (tailLets) =
          nonCheckingTranslateList(
            temputs, fate, patterns1.tail, patternInputExprs2.tail)
        (headLets ++ tailLets)
      }
      case _ => vfail("wat")
    }
  }

  // Note: This will unlet/drop the input expression. Be warned.
  def nonCheckingInferAndTranslate(
      temputs: TemputsBox,
      fate: FunctionEnvironmentBox,
      rules: List[IRulexAR],
      typeByRune: Map[String, ITemplataType],
      pattern: AtomSP,
      inputExpr: ReferenceExpression2):
  (List[ReferenceExpression2]) = {

    val templatasByRune =
      InferTemplar.inferFromArgCoords(fate.snapshot, temputs, List(), rules, typeByRune, List(pattern), None, List(), List(ParamFilter(inputExpr.resultRegister.reference, None))) match {
        case (isf @ InferSolveFailure(_, _, _, _, _, _)) => vfail("Couldn't figure out runes for pattern!\n" + isf)
        case (InferSolveSuccess(tbr)) => (tbr.templatasByRune.mapValues(v => List(TemplataEnvEntry(v))))
      }

    fate.addEntries(templatasByRune)

    innerNonCheckingTranslate(
      temputs, fate, pattern, inputExpr)
  }

  // Note: This will unlet/drop the input expression. Be warned.
  def nonCheckingTranslate(
      temputs: TemputsBox,
      fate: FunctionEnvironmentBox,
      pattern: AtomSP,
      inputExpr: ReferenceExpression2):
  (List[ReferenceExpression2]) = {
    innerNonCheckingTranslate(
      temputs, fate, pattern, inputExpr)
  }

  // the #1 case above
  // returns:
  // - the temputs
  // - the new seq num
  // - a bunch of lets.
  // - exports, to toss into the env
  // - function state
  private def innerNonCheckingTranslate(
      temputs: TemputsBox,
      fate: FunctionEnvironmentBox,
      pattern: AtomSP,
      unconvertedInputExpr: ReferenceExpression2):
  (List[ReferenceExpression2]) = {


    val AtomSP(maybeCapture, maybeVirtuality, coordRune, maybeDestructure) = pattern

    if (maybeVirtuality.nonEmpty) {
      // This is actually to be expected for when we translate the patterns from the
      // function's parameters. Ignore them.
    }

    val expectedTemplata = fate.getNearestTemplataWithName(coordRune, Set(TemplataLookupContext))
    val expectedCoord =
      expectedTemplata match {
        case Some(CoordTemplata(coord)) => coord
        case Some(_) => vfail("not a coord!")
        case None => vfail("not found!")
      }

    // Now we convert m to a Marine. This also checks that it *can* be
    // converted to a Marine.
    val inputExpr =
      TypeTemplar.convert(fate.snapshot, temputs, unconvertedInputExpr, expectedCoord);

    val (inputOrLookupExpr, lets0) =
      maybeCapture match {
        case None => {
          (inputExpr, List())
        }
        case Some(CaptureP(name, variability)) => {
          val variableId = VariableId2(fate.function.lambdaNumber, name)

          val export =
            ExpressionTemplar.makeUserLocalVariable(
              temputs, fate, variableId, Conversions.evaluateVariability(variability), expectedCoord)
          val let = LetNormal2(export, inputExpr);

          val localLookupExpr =
            ExpressionTemplar.borrowSoftLoad(
              temputs, LocalLookup2(export, inputExpr.resultRegister.reference))
          fate.addVariable(export)

          (localLookupExpr, List(let))
        }
      }

    maybeDestructure match {
      case None => {
        (lets0)
      }
      case Some(listOfMaybeDestructureMemberPatterns) => {
        expectedCoord.referend match {
          case StructRef2(_) => {
            // Example:
            //   struct Marine { bork: Bork; }
            //   Marine(b) = m;
            // In this case, expectedStructType1 = TypeName1("Marine") and
            // destructureMemberPatterns = List(CaptureSP("b", FinalP, None)).
            // Since we're receiving an owning reference, and we're *not* capturing
            // it in a variable, it will be destroyed and we will harvest its parts.

            val (innerLets) =
              nonCheckingTranslateStructInner(
                temputs, fate, listOfMaybeDestructureMemberPatterns, expectedCoord, inputOrLookupExpr)
            (lets0 ++ innerLets)
          }
          case PackT2(_, _) => {
            val (innerLets) =
            nonCheckingTranslatePack(
              temputs, fate, listOfMaybeDestructureMemberPatterns, inputExpr)
            (lets0 ++ innerLets)
          }
          case _ => vfail("impl!")
        }
      }
    }
//
//    pattern match {
//      case TypeOfSP(TemplateCallT1(templateName, templateArgTypes1)) => {
//        val expectedCitizenRef2 =
//          TypeTemplar.callTemplate(env, temputs, templateName, templateArgTypes1)
//
//        // Our resulting variable will have this ownership
//        val expectedCitizenDef2 =
//          expectedCitizenRef2 match {
//            case ReferendTemplata(sr @ StructRef2(_)) => temputs.lookupCitizen(sr)
//            case ReferendTemplata(ir @ InterfaceRef2(_)) => temputs.lookupCitizen(ir)
//          }
//
//        val expectedOwnership =
//          if (expectedCitizenDef2.mutability == ImmutableP) {
//            Share
//          } else {
////            if (expectBorrow) Borrow else Own
//            Own
//          }
//
//        val expectedPointerType = Coord(expectedOwnership, expectedCitizenDef2.getRef)
//
//        // Don't need output, since we're just doing a compile time check here
//        TypeTemplar.convert(env, temputs, inputExpr, expectedPointerType)
//
//        (temputs, fate, List(), List())
//      }
//      case TypeOfSP(type1) => {
//        val unborrowedTargetReference =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs, type1, Own)
//        // If we expect a borrow, then here we make a targetReference that reflects that
//        val targetReference =
//          if (unborrowedTargetReference.ownership == Share) {
//            unborrowedTargetReference
//          } else {
////            if (expectBorrow) {
////              Coord(Borrow, unborrowedTargetReference.referend)
////            } else {
//              unborrowedTargetReference
////            }
//          }
//
//        // Don't need output, since we're just doing a compile time check here
//        TypeTemplar.convert(env, temputs, inputExpr, targetReference)
//
//        (temputs, fate, List(), List())
//      }
//      case CaptureSP(name, variability, _, None) => already moved
//      case CaptureSP(name, variability, _, Some(TypeOfSP(expectedType1))) => {
//        // Example:
//        //   struct Marine { bork: Bork; }
//        //   Marine(b : Bork) = m;
//        // In this case, name = 'b' and inner1 = 'Bork'
//
//        // This is local variable b
//        val variableId = VariableId2(env.currentFunction1.get.lambdaNumber, name)
//        // This is where we figure out that b should be an owning Bork
//        val expectedPointerType =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs, expectedType1, Own)
//        // Now we convert Marine's first member to a Bork. This also checks that
//        // it *can* be converted to a Bork.
//        val convertedInputLookupExpr =
//          TypeTemplar.convert(env, temputs, inputExpr, expectedPointerType);
//        // Now we make the local variable b
//        val newExport =
//          ExpressionTemplar.makeUserLocalVariable(
//            env, temputs, variableId, variability, convertedInputLookupExpr.resultRegister.reference)
//        val newLet = LetNormal2(newExport, convertedInputLookupExpr)
//
//        (temputs, fate, List(newLet), List(newExport))
//      }
//      case CaptureSP(name, variability, _, Some(DestructureSP(expectedStructType1, destructureMemberPatterns))) => {
//        // Example:
//        //   struct Marine { bork: Bork; }
//        //   m : Marine(b) = inMarine;
//        // In this case, name = 'm', expectedStructType1 = TypeName1("Marine"),
//        // and destructureMemberPatterns = List(CaptureSP("b", FinalP, None)).
//        // The local m is actually an owning reference, and things inside the
//        // Marine (like b) have to be borrow references (the other way wouldnt
//        // make sense, what would b point to? A dead object?).
//
//        // This is local variable m
//        val variableId = VariableId2(env.currentFunction1.get.lambdaNumber, name)
//        // This is where we figure out that m should be an owning Marine
//        val (expectedPointerType @ Coord(_, StructRef2(_))) =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs, expectedStructType1, Own)
//        // Now we convert inMarine to a Marine. This also checks that
//        // it *can* be converted to a Marine.
//        val convertedInputLookupExpr =
//          TypeTemplar.convert(env, temputs, inputExpr, expectedPointerType);
//        // Now we make the local variable m
//        val newExport =
//          ExpressionTemplar.makeUserLocalVariable(
//            env, temputs, variableId, variability, convertedInputLookupExpr.resultRegister.reference)
//        val newLet = LetNormal2(newExport, convertedInputLookupExpr)
//
//        // The input to the inner destructure expressions will be a borrow of m.
//        val innerInputLookup = LocalLookup2(newExport, expectedPointerType)
//        val innerInputBorrow =
//          ExpressionTemplar.borrowSoftLoad(temputs, innerInputLookup)
//
//        val (innerLets, innerExports) =
//          nonCheckingTranslateStructInner(env, temputs, fate, patternId, destructureMemberPatterns, expectedPointerType, innerInputBorrow)
//
//        (temputs, fate, newLet :: innerLets, newExport :: innerExports)
//      }
//      case d @ DestructureSP(expectedStructType1, destructureMemberPatterns) => already moved
//      case p @ PackSP(_) => already moved
//      case _ => {
//        vfail("not yet " + pattern.toString)
//      }
//    }
  }

  private def nonCheckingTranslatePack(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
    innerPatternMaybes: List[Option[AtomSP]],
    inputPackExpr: ReferenceExpression2):
  (List[ReferenceExpression2]) = {
    // we gotta:
    // destructure the incoming pack expression into a bunch of locals.
    // for each member, unlet its local and pass it to the subpattern.

    val packRef2 = inputPackExpr.resultRegister.reference
    val Coord(packOwnership, PackT2(_, underlyingStruct @ StructRef2(_))) = packRef2

    val structType2 = Coord(packOwnership, underlyingStruct)

    val reinterpretExpr2 =
      TemplarReinterpret2(inputPackExpr, structType2)

    nonCheckingTranslateStructInner(
      temputs, fate, innerPatternMaybes, structType2, reinterpretExpr2)
  }

  private def nonCheckingTranslateStructInner(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
    innerPatternMaybes: List[Option[AtomSP]],
    structType2: Coord,
    inputStructExpr: ReferenceExpression2):
  (List[ReferenceExpression2]) = {
    val Coord(structOwnership, structRef2 @ StructRef2(_)) = structType2
    val structDef2 = temputs.structDefsByRef(structRef2)
    // We don't pattern match against closure structs.
    val memberTypes = structDef2.members.map(_.tyype.expectReferenceMember().reference)

    val (counter) = fate.nextVarCounter()

    structOwnership match {
      case Own => {
        val (memberLocalVariables) =
          memberTypes.zipWithIndex.map({
            case ((memberType, index)) => {
              val variableId = VariableId2(fate.function.lambdaNumber, "__pack_" + counter + "_member_" + index)
              val localVariable = ReferenceLocalVariable2(variableId, Final, memberType)
              fate.addVariable(localVariable)
              localVariable
            }
          })

        val destructure = Destructure2(inputStructExpr, structRef2, memberLocalVariables)

        val (lets) =
          innerPatternMaybes.zip(memberLocalVariables).flatMap({
            case ((None, localVariable)) => {
              val (unletExpr) =
                ExpressionTemplar.unletLocal(fate, localVariable)
              List(DestructorTemplar.drop(fate, temputs, unletExpr))
            }
            case ((Some(innerPattern), localVariable)) => {
              val (unletExpr) =
                ExpressionTemplar.unletLocal(fate, localVariable)
              innerNonCheckingTranslate(temputs, fate, innerPattern, unletExpr)
            }
          })
        (destructure :: lets)
      }
      case Share => {
        // This is different from the Own case because we're not destructuring the incoming thing, we're just
        // loading from it.

        val packLocalVarName = "__pack_" + counter
        val packLocalVariableId = VariableId2(fate.function.lambdaNumber, packLocalVarName)
        val packLocalVariable = ReferenceLocalVariable2(packLocalVariableId, Final, structType2)
        val packLet = LetNormal2(packLocalVariable, inputStructExpr);
        fate.addVariable(packLocalVariable)

        val (innerLets) =
          innerPatternMaybes.zip(memberTypes).zipWithIndex
            .flatMap({
              case (((None, _), _)) => {
                List()
              }
              case (((Some(innerPattern), memberType), index)) => {
                val loadExpr =
                  SoftLoad2(
                    ReferenceMemberLookup2(
                      SoftLoad2(LocalLookup2(packLocalVariable, structType2), Share),
                      index.toString, memberType),
                    Share)
                innerNonCheckingTranslate(temputs, fate, innerPattern, loadExpr)
              }
            })

        val (packUnlet) = ExpressionTemplar.unletLocal(fate, packLocalVariable)
        val (dropExpr) =
          DestructorTemplar.drop(fate, temputs, packUnlet)

        ((packLet :: innerLets) :+ dropExpr)
      }
      case Borrow => {
        // here, instead of doing a destructure, we'd just put this in a variable
        // and do a bunch of lookups on it.
        vfail("implement!")
      }
    }
  }
//
//  // Assumes the templated stuff has already been put into this environment
//  def getParameterType2(env: IEnvironmentBox, temputs: TemputsBox, param1: AtomSP):
//  (Temputs, Coord) = {
//    val type1 = getPatternType1(param1)
//    val type2 = TypeTemplar.evaluateType(env, temputs, type1)
//    (temputs, TemplataTemplar.coerceTemplataToReference(temputs, type2, Own))
//  }
//
//  // Assumes the templated stuff has already been put into this environment
//  def getPatternType2(env: IEnvironmentBox, temputs: TemputsBox, pattern1: AtomSP):
//  (Temputs, Coord) = {
//    val type1 = getPatternType1(pattern1)
//    val type2 =
//      TypeTemplar.evaluateType(env, temputs, type1)
//    (temputs, TemplataTemplar.coerceTemplataToReference(temputs, type2, Own))
//  }
//
//  def getParameterType1(param1: AtomSP): ITemplexS = {
//    getPatternType1(param1)
//  }
//
//  def getParameterType1s(params1: List[AtomSP]): List[ITemplexS] = {
//    getPatternType1s(params1)
//  }
//
//  def getPatternType1s(patterns1: List[AtomSP]):
//  List[ITemplexS] = {
//    patterns1.map(pattern1 => getPatternType1(pattern1))
//  }

  // Once we know that a function isnt templated, we use this to figure out
  // the types of its AtomSP
//  def getPatternType1(pattern: AtomSP):
//      ITemplexS = {
//    pattern.coordPattern.get.iname match {
//      case TemplexSP(name) =>
//      case TypeOfSP(type1) => type1
//      case CaptureSP(name, mutable, _, Some(inner1)) => {
//        getPatternType1(inner1)
//      }
//      case PackSP(elements0) => {
//        PackT1(getPatternType1s(elements0))
//      }
//      case CaptureSP(name, mutable, _, None) => {
//        // we should already know that this isn't templated
//        vfail("wat")
//      }
//      case _ => vfail("wat " + pattern)
//    }
//  }

//  private def getPatternsCaptureDeclarations(patterns: List[AtomSP]):
//      VariableDeclarations = {
//    patterns.foldLeft(VariableDeclarations(Set()))({ case (previousDeclarations, pattern) =>
//      previousDeclarations ++ getPatternCaptureDeclarations(pattern)
//    })
//  }
}