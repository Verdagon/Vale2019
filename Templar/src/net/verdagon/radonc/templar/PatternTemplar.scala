package net.verdagon.radonc.templar;

import net.verdagon.radonc.astronomer.{IRulexAR, ITemplataType}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.CaptureP
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules.IRulexSR
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.DestructorTemplar
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.templar.templata.TemplataTemplar
import net.verdagon.radonc.vfail

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
    temputs0: Temputs,
    fate0: FunctionEnvironment,
    rules: List[IRulexAR],
    typeByRune: Map[String, ITemplataType],
    patterns1: List[AtomSP],
    patternInputExprs2: List[ReferenceExpression2]):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {

    val patternInputCoords = patternInputExprs2.map(_.resultRegister.reference)

    val (temputs1, templatasByRune) =
      InferTemplar.inferFromArgCoords(fate0, temputs0, List(), rules, typeByRune, patterns1, None, List(), patternInputCoords.map(arg => ParamFilter(arg, None))) match {
        case (_, InferSolveFailure(_, _, _, _, _, _)) => vfail("Couldn't figure out runes for pattern!")
        case (t, InferSolveSuccess(tbr)) => (t, tbr.templatasByRune.mapValues(v => List(TemplataEnvEntry(v))))
      }

    val fate1 = fate0.addEntries(templatasByRune)

    nonCheckingTranslateList(temputs1, fate1, patterns1, patternInputExprs2)
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
    temputs0: Temputs,
    fate0: FunctionEnvironment,
      patterns1: List[AtomSP],
      patternInputExprs2: List[ReferenceExpression2]):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {

    patterns1.zip(patternInputExprs2) match {
      case Nil => (temputs0, fate0, Nil)
      case (pattern1, patternInputExpr2) :: _ => {
        val (temputs2, fate1, headLets) =
          PatternTemplar.innerNonCheckingTranslate(
            temputs0, fate0, pattern1, patternInputExpr2);
        val (temputs3, fate2, tailLets) =
          nonCheckingTranslateList(
            temputs2, fate1, patterns1.tail, patternInputExprs2.tail)
        (temputs3, fate2, headLets ++ tailLets)
      }
      case _ => vfail("wat")
    }
  }

  // Note: This will unlet/drop the input expression. Be warned.
  def nonCheckingInferAndTranslate(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      rules: List[IRulexAR],
      typeByRune: Map[String, ITemplataType],
      pattern: AtomSP,
      inputExpr: ReferenceExpression2):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {

    val (temputs1, templatasByRune) =
      InferTemplar.inferFromArgCoords(fate0, temputs0, List(), rules, typeByRune, List(pattern), None, List(), List(ParamFilter(inputExpr.resultRegister.reference, None))) match {
        case (_, isf @ InferSolveFailure(_, _, _, _, _, _)) => vfail("Couldn't figure out runes for pattern!\n" + isf)
        case (t, InferSolveSuccess(tbr)) => (t, tbr.templatasByRune.mapValues(v => List(TemplataEnvEntry(v))))
      }

    val fate1 = fate0.addEntries(templatasByRune)

    innerNonCheckingTranslate(
      temputs1, fate1, pattern, inputExpr)
  }

  // Note: This will unlet/drop the input expression. Be warned.
  def nonCheckingTranslate(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      pattern: AtomSP,
      inputExpr: ReferenceExpression2):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {
    innerNonCheckingTranslate(
      temputs0, fate0, pattern, inputExpr)
  }

  // the #1 case above
  // returns:
  // - the temputs
  // - the new seq num
  // - a bunch of lets.
  // - exports, to toss into the env
  // - function state
  private def innerNonCheckingTranslate(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      pattern: AtomSP,
      unconvertedInputExpr: ReferenceExpression2):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {


    val AtomSP(maybeCapture, maybeVirtuality, coordRune, maybeDestructure) = pattern

    if (maybeVirtuality.nonEmpty) {
      // This is actually to be expected for when we translate the patterns from the
      // function's parameters. Ignore them.
    }

    val expectedTemplata = fate0.getNearestTemplataWithName(coordRune, Set(TemplataLookupContext))
    val expectedCoord =
      expectedTemplata match {
        case Some(CoordTemplata(coord)) => coord
        case Some(_) => vfail("not a coord!")
        case None => vfail("not found!")
      }

    // Now we convert m to a Marine. This also checks that it *can* be
    // converted to a Marine.
    val (temputs1, inputExpr) =
      TypeTemplar.convert(fate0, temputs0, unconvertedInputExpr, expectedCoord);

    val (fate1, inputOrLookupExpr, lets0) =
      maybeCapture match {
        case None => {
          (fate0, inputExpr, List())
        }
        case Some(CaptureP(name, variability)) => {
          val variableId = VariableId2(fate0.function.lambdaNumber, name)

          val export =
            ExpressionTemplar.makeUserLocalVariable(
              temputs1, fate0, variableId, Conversions.evaluateVariability(variability), expectedCoord)
          val let = LetNormal2(export, inputExpr);

          val localLookupExpr =
            ExpressionTemplar.borrowSoftLoad(
              temputs1, LocalLookup2(export, inputExpr.resultRegister.reference))

          (fate0.addVariable(export), localLookupExpr, List(let))
        }
      }

    maybeDestructure match {
      case None => {
        (temputs1, fate1, lets0)
      }
      case Some(listOfMaybeDestructureMemberPatterns) => {
        expectedCoord.referend match {
          case StructRef2(_) => {
            // Example:
            //   struct Marine { bork: Bork; }
            //   let Marine(b) = m;
            // In this case, expectedStructType1 = TypeName1("Marine") and
            // destructureMemberPatterns = List(CaptureSP("b", FinalP, None)).
            // Since we're receiving an owning reference, and we're *not* capturing
            // it in a variable, it will be destroyed and we will harvest its parts.

            val (temputs2, fate2, innerLets) =
              nonCheckingTranslateStructInner(
                temputs1, fate1, listOfMaybeDestructureMemberPatterns, expectedCoord, inputOrLookupExpr)
            (temputs2, fate2, lets0 ++ innerLets)
          }
          case PackT2(_, _) => {
            val (temputs2, fate2, innerLets) =
            nonCheckingTranslatePack(
              temputs1, fate1, listOfMaybeDestructureMemberPatterns, inputExpr)
            (temputs2, fate2, lets0 ++ innerLets)
          }
          case _ => vfail("impl!")
        }
      }
    }
//
//    pattern match {
//      case TypeOfSP(TemplateCallT1(templateName, templateArgTypes1)) => {
//        val (temputs2, expectedCitizenRef2) =
//          TypeTemplar.callTemplate(env, temputs0, templateName, templateArgTypes1)
//
//        // Our resulting variable will have this ownership
//        val expectedCitizenDef2 =
//          expectedCitizenRef2 match {
//            case ReferendTemplata(sr @ StructRef2(_)) => temputs2.lookupCitizen(sr)
//            case ReferendTemplata(ir @ InterfaceRef2(_)) => temputs2.lookupCitizen(ir)
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
//        TypeTemplar.convert(env, temputs2, inputExpr, expectedPointerType)
//
//        (temputs2, fate0, List(), List())
//      }
//      case TypeOfSP(type1) => {
//        val (temputs1, unborrowedTargetReference) =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs0, type1, Own)
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
//        TypeTemplar.convert(env, temputs1, inputExpr, targetReference)
//
//        (temputs1, fate0, List(), List())
//      }
//      case CaptureSP(name, variability, _, None) => already moved
//      case CaptureSP(name, variability, _, Some(TypeOfSP(expectedType1))) => {
//        // Example:
//        //   struct Marine { bork: Bork; }
//        //   let Marine(b : Bork) = m;
//        // In this case, name = 'b' and inner1 = 'Bork'
//
//        // This is local variable b
//        val variableId = VariableId2(env.currentFunction1.get.lambdaNumber, name)
//        // This is where we figure out that b should be an owning Bork
//        val (temputs1, expectedPointerType) =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs0, expectedType1, Own)
//        // Now we convert Marine's first member to a Bork. This also checks that
//        // it *can* be converted to a Bork.
//        val convertedInputLookupExpr =
//          TypeTemplar.convert(env, temputs1, inputExpr, expectedPointerType);
//        // Now we make the local variable b
//        val newExport =
//          ExpressionTemplar.makeUserLocalVariable(
//            env, temputs1, variableId, variability, convertedInputLookupExpr.resultRegister.reference)
//        val newLet = LetNormal2(newExport, convertedInputLookupExpr)
//
//        (temputs1, fate0, List(newLet), List(newExport))
//      }
//      case CaptureSP(name, variability, _, Some(DestructureSP(expectedStructType1, destructureMemberPatterns))) => {
//        // Example:
//        //   struct Marine { bork: Bork; }
//        //   let m : Marine(b) = inMarine;
//        // In this case, name = 'm', expectedStructType1 = TypeName1("Marine"),
//        // and destructureMemberPatterns = List(CaptureSP("b", FinalP, None)).
//        // The local m is actually an owning reference, and things inside the
//        // Marine (like b) have to be borrow references (the other way wouldnt
//        // make sense, what would b point to? A dead object?).
//
//        // This is local variable m
//        val variableId = VariableId2(env.currentFunction1.get.lambdaNumber, name)
//        // This is where we figure out that m should be an owning Marine
//        val (temputs1, expectedPointerType @ Coord(_, StructRef2(_))) =
//          TypeTemplar.evaluateAndReferencifyType(
//            env, temputs0, expectedStructType1, Own)
//        // Now we convert inMarine to a Marine. This also checks that
//        // it *can* be converted to a Marine.
//        val convertedInputLookupExpr =
//          TypeTemplar.convert(env, temputs1, inputExpr, expectedPointerType);
//        // Now we make the local variable m
//        val newExport =
//          ExpressionTemplar.makeUserLocalVariable(
//            env, temputs1, variableId, variability, convertedInputLookupExpr.resultRegister.reference)
//        val newLet = LetNormal2(newExport, convertedInputLookupExpr)
//
//        // The input to the inner destructure expressions will be a borrow of m.
//        val innerInputLookup = LocalLookup2(newExport, expectedPointerType)
//        val innerInputBorrow =
//          ExpressionTemplar.borrowSoftLoad(temputs1, innerInputLookup)
//
//        val (temputs2, fate2, innerLets, innerExports) =
//          nonCheckingTranslateStructInner(env, temputs1, fate0, patternId, destructureMemberPatterns, expectedPointerType, innerInputBorrow)
//
//        (temputs2, fate2, newLet :: innerLets, newExport :: innerExports)
//      }
//      case d @ DestructureSP(expectedStructType1, destructureMemberPatterns) => already moved
//      case p @ PackSP(_) => already moved
//      case _ => {
//        vfail("not yet " + pattern.toString)
//      }
//    }
  }

  private def nonCheckingTranslatePack(
    temputs0: Temputs,
    fate0: FunctionEnvironment,
    innerPatternMaybes: List[Option[AtomSP]],
    inputPackExpr: ReferenceExpression2):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {
    // we gotta:
    // destructure the incoming pack expression into a bunch of locals.
    // for each member, unlet its local and pass it to the subpattern.

    val packRef2 = inputPackExpr.resultRegister.reference
    val Coord(packOwnership, PackT2(_, underlyingStruct @ StructRef2(_))) = packRef2

    val structType2 = Coord(packOwnership, underlyingStruct)

    val reinterpretExpr2 =
      TemplarReinterpret2(inputPackExpr, structType2)

    nonCheckingTranslateStructInner(
      temputs0, fate0, innerPatternMaybes, structType2, reinterpretExpr2)
  }

  private def nonCheckingTranslateStructInner(
    temputs0b: Temputs,
    fate0: FunctionEnvironment,
    innerPatternMaybes: List[Option[AtomSP]],
    structType2: Coord,
    inputStructExpr: ReferenceExpression2):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2]) = {
    val Coord(structOwnership, structRef2 @ StructRef2(_)) = structType2
    val structDef2 = temputs0b.structDefsByRef(structRef2)
    // We don't pattern match against closure structs.
    val memberTypes = structDef2.members.map(_.tyype.expectReferenceMember().reference)

    val (fate1, counter) = fate0.nextVarCounter()

    structOwnership match {
      case Own => {
        val (fate3, memberLocalVariables) =
          memberTypes.zipWithIndex.foldLeft((fate1, List[ReferenceLocalVariable2]()))({
            case ((fate2, memberVariablesSoFar), (memberType, index)) => {
              val variableId = VariableId2(fate2.function.lambdaNumber, "__pack_" + counter + "_member_" + index)
              val localVariable = ReferenceLocalVariable2(variableId, Final, memberType)
              (fate2.addVariable(localVariable), memberVariablesSoFar :+ localVariable)
            }
          })

        val destructure = Destructure2(inputStructExpr, structRef2, memberLocalVariables)

        val (temputs4, fate10, lets) =
          innerPatternMaybes.zip(memberLocalVariables)
            .foldLeft(temputs0b, fate3, List[ReferenceExpression2]())({
              case ((temputs1, fate5, previousExprs), (None, localVariable)) => {
                val (fate6, unletExpr) =
                  ExpressionTemplar.unletLocal(fate5, localVariable)
                val (temputs2, fate7, dropExpr) =
                  DestructorTemplar.drop(fate6, temputs1, unletExpr)
                (temputs2, fate7, previousExprs :+ dropExpr)
              }
              case ((temputs1, fate5, previousExprs), (Some(innerPattern), localVariable)) => {
                val (fate6, unletExpr) =
                  ExpressionTemplar.unletLocal(fate5, localVariable)
                val (temputs3, fate8, innerExprs) =
                  innerNonCheckingTranslate(
                    temputs1, fate6, innerPattern, unletExpr)
                (temputs3, fate8, previousExprs ++ innerExprs)
              }
            })

        (temputs4, fate10, destructure :: lets)
      }
      case Share => {
        // This is different from the Own case because we're not destructuring the incoming thing, we're just
        // loading from it.

        val packLocalVarName = "__pack_" + counter
        val packLocalVariableId = VariableId2(fate1.function.lambdaNumber, packLocalVarName)
        val packLocalVariable = ReferenceLocalVariable2(packLocalVariableId, Final, structType2)
        val packLet = LetNormal2(packLocalVariable, inputStructExpr);
        val fate2 = fate1.addVariable(packLocalVariable)

        val (temputs4, fate10, innerLets) =
          innerPatternMaybes.zip(memberTypes).zipWithIndex
            .foldLeft(temputs0b, fate2, List[ReferenceExpression2]())({
              case ((temputs1, fate5, previousExprs), ((None, _), _)) => {
                (temputs1, fate5, previousExprs)
              }
              case ((temputs1, fate5, previousExprs), ((Some(innerPattern), memberType), index)) => {
                val loadExpr =
                  SoftLoad2(
                    ReferenceMemberLookup2(
                      SoftLoad2(LocalLookup2(packLocalVariable, structType2), Share),
                      index.toString, memberType),
                    Share)
                val (temputs3, fate9, innerExprs) =
                  innerNonCheckingTranslate(
                    temputs1, fate5, innerPattern, loadExpr)
                (temputs3, fate9, previousExprs ++ innerExprs)
              }
            })

        val (fate11, packUnlet) = ExpressionTemplar.unletLocal(fate10, packLocalVariable)
        val (temputs5, fate12, dropExpr) =
          DestructorTemplar.drop(fate11, temputs4, packUnlet)

        (temputs5, fate12, (packLet :: innerLets) :+ dropExpr)
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
//  def getParameterType2(env: IEnvironment, temputs0: Temputs, param1: AtomSP):
//  (Temputs, Coord) = {
//    val type1 = getPatternType1(param1)
//    val (temputs2, type2) = TypeTemplar.evaluateType(env, temputs0, type1)
//    (temputs2, TemplataTemplar.coerceTemplataToReference(temputs2, type2, Own))
//  }
//
//  // Assumes the templated stuff has already been put into this environment
//  def getPatternType2(env: IEnvironment, temputs0: Temputs, pattern1: AtomSP):
//  (Temputs, Coord) = {
//    val type1 = getPatternType1(pattern1)
//    val (temputs2, type2) =
//      TypeTemplar.evaluateType(env, temputs0, type1)
//    (temputs2, TemplataTemplar.coerceTemplataToReference(temputs2, type2, Own))
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
