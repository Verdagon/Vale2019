package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS, PatternSUtils}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer}
import net.verdagon.vale._

import scala.collection.immutable.List

object StructTemplar {
  def addBuiltInStructs(env: NamespaceEnvironment[IName2], temputs: TemputsBox): (StructRef2) = {
    StructTemplarCore.addBuiltInStructs(env, temputs)
  }

  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
    Map(
      "structConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironment,
            temputs: TemputsBox,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (FunctionHeader2) = {
            val Some(Coord(_, structRef2 @ StructRef2(_))) = maybeRetCoord
            val structDef2 = temputs.lookupStruct(structRef2)
            StructTemplarCore.makeStructConstructor(temputs, originFunction, structDef2, env.fullName)
          }
        },
      "interfaceConstructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            env: FunctionEnvironment,
            temputs: TemputsBox,
            originFunction: Option[FunctionA],
            paramCoords: List[Parameter2],
            maybeRetCoord: Option[Coord]):
          (FunctionHeader2) = {
            // The interface should be in the "__Interface" rune of the function environment.
            val interfaceRef2 =
              env.getNearestTemplataWithAbsoluteName2(AnonymousSubstructParentInterfaceRune2(), Set(TemplataLookupContext)) match {
                case Some(KindTemplata(ir @ InterfaceRef2(_))) => ir
                case _ => vwat()
              }

            val constructorFullName = env.fullName
            val (structRef2, _) =
              StructTemplar.makeAnonymousSubstruct(
                temputs, interfaceRef2, paramCoords.map(_.tyype))

            val structDef = temputs.lookupStruct(structRef2)
            val constructor =
              StructTemplar.makeStructConstructor(
                temputs, originFunction, structDef, constructorFullName)

            constructor
          }
        })
  }

  def getConstructor(struct1: StructA): FunctionA = {
    println("todo: put all the members' rules up in the top of the struct")
    val params =
      struct1.members.zipWithIndex.map({
        case (member, index) => {
          ParameterA(
            AtomAP(
              CaptureA(CodeVarNameA(member.name), FinalP),
              None,
              MemberRuneA(index),
              None))
        }
      })
    val retRune = ReturnRuneA()
    val rules =
      struct1.rules :+
      EqualsAR(
        TemplexAR(RuneAT(retRune, CoordTemplataType)),
        TemplexAR(
          if (struct1.isTemplate) {
            CallAT(
              AbsoluteNameAT(struct1.name, struct1.tyype),
              struct1.identifyingRunes.map(rune => RuneAT(rune, struct1.typeByRune(rune))),
              CoordTemplataType)
          } else {
            AbsoluteNameAT(struct1.name, CoordTemplataType)
          }))

    FunctionA(
      ConstructorNameA(struct1.name),
      true,
      struct1.tyype match {
        case KindTemplataType => FunctionTemplataType
        case TemplateTemplataType(params, KindTemplataType) => TemplateTemplataType(params, FunctionTemplataType)
      },
      struct1.identifyingRunes,
      struct1.typeByRune + (retRune -> CoordTemplataType),
      params,
      Some(retRune),
      rules,
      GeneratedBodyA("structConstructorGenerator"))
  }

  def getInterfaceConstructor(interfaceA: InterfaceA): FunctionA = {
    println("todo: put all the members' rules up in the top of the struct")
    val identifyingRunes =
      interfaceA.identifyingRunes ++ interfaceA.internalMethods.indices.map(x => CodeRuneA("Functor" + x))
    val typeByRune =
      interfaceA.typeByRune ++
      interfaceA.internalMethods.indices.map(i => (CodeRuneA("Functor" + i) -> CoordTemplataType)).toMap +
        (AnonymousSubstructParentInterfaceRuneA() -> KindTemplataType)
    val params =
      interfaceA.internalMethods.indices.toList.map(index => {
        ParameterA(
          AtomAP(
            CaptureA(AnonymousSubstructMemberNameA(index), FinalP),
            None,
            CodeRuneA("Functor" + index),
            None))
      })
    val rules =
      interfaceA.rules :+
//        EqualsAR(
//          TemplexAR(RuneAT(retRune, CoordTemplataType)),
//          TemplexAR(
//            if (interfaceA.isTemplate) {
//              CallAT(
//                NameAT(interfaceA.name, interfaceA.tyype),
//                interfaceA.identifyingRunes.map(rune => RuneAT(rune, interfaceA.typeByRune(rune))),
//                CoordTemplataType)
//            } else {
//              NameAT(interfaceA.name, CoordTemplataType)
//            })) :+
    // We stash the interface type in the env, so that when the interface constructor generator runs,
    // it can read this to know what interface it's making a subclass of.
      EqualsAR(
        TemplexAR(RuneAT(AnonymousSubstructParentInterfaceRuneA(), KindTemplataType)),
        TemplexAR(
          if (interfaceA.isTemplate) {
            CallAT(
              AbsoluteNameAT(interfaceA.name, interfaceA.tyype),
              interfaceA.identifyingRunes.map(rune => RuneAT(rune, interfaceA.typeByRune(rune))),
              KindTemplataType)
          } else {
            AbsoluteNameAT(interfaceA.name, KindTemplataType)
          }))

    val TopLevelCitizenDeclarationNameA(name, codeLocation) = interfaceA.name
    FunctionA(
      FunctionNameA(name, codeLocation),
      true,
      interfaceA.tyype match {
        case KindTemplataType => FunctionTemplataType
        case TemplateTemplataType(params, KindTemplataType) => TemplateTemplataType(params, FunctionTemplataType)
      },
      identifyingRunes,
      typeByRune,
      params,
      None,
      rules,
      GeneratedBodyA("interfaceConstructorGenerator"))
  }

  def getStructRef(
    temputs: TemputsBox,
    structTemplata: StructTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (StructRef2) = {
    StructTemplarTemplateArgsLayer.getStructRef(
      temputs, structTemplata, uncoercedTemplateArgs)
  }

  def getInterfaceRef(
    temputs: TemputsBox,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceTemplata,
    uncoercedTemplateArgs: List[ITemplata]):
  (InterfaceRef2) = {
    StructTemplarTemplateArgsLayer.getInterfaceRef(
      temputs, interfaceTemplata, uncoercedTemplateArgs)
  }

  def convert(
      env: IEnvironment,
      temputs: TemputsBox,
      sourceExpr: ReferenceExpression2,
      sourceStructRef: StructRef2,
      targetInterfaceRef: InterfaceRef2):
  (ReferenceExpression2) = {
    ImplTemplar.isAncestor(temputs, sourceStructRef, targetInterfaceRef) match {
      case (true) => (StructToInterfaceUpcast2(sourceExpr, targetInterfaceRef))
      case (false) => vfail("Can't upcast a " + sourceStructRef + " to a " + targetInterfaceRef)
    }
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: TemputsBox,
    name: LambdaNameA,
    functionS: FunctionA,
    members: List[StructMember2]):
  (StructRef2, Mutability, FunctionTemplata) = {
    StructTemplarTemplateArgsLayer.makeClosureUnderstruct(containingFunctionEnv, temputs, name, functionS, members)
  }

  // Makes a struct to back a pack or tuple
  def makeSeqOrPackUnderstruct(env: NamespaceEnvironment[IName2], temputs: TemputsBox, memberTypes2: List[Coord], name: ICitizenName2):
  (StructRef2, Mutability) = {
    StructTemplarTemplateArgsLayer.makeSeqOrPackUnerstruct(env, temputs, memberTypes2, name)
  }

  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
  def makeAnonymousSubstruct(
    temputs: TemputsBox,
    interfaceRef2: InterfaceRef2,
    lambdas: List[Coord]):
  (StructRef2, Mutability) = {
    val interfaceEnv = vassertSome(temputs.envByInterfaceRef.get(interfaceRef2))
    StructTemplarTemplateArgsLayer.makeAnonymousSubstruct(
        interfaceEnv, temputs, interfaceRef2, lambdas)
  }

  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
  // This does NOT make a constructor, because its so easy to just Construct2 it.
  def prototypeToAnonymousStruct(
    temputs: TemputsBox,
    prototype: Prototype2):
  StructRef2 = {
    val structFullName = prototype.fullName.addStep(LambdaCitizenName2(CodeLocation2(0, 0)))
    val outerEnv = temputs.envByFunctionSignature(prototype.toSignature)
    StructTemplarTemplateArgsLayer.prototypeToAnonymousStruct(
      outerEnv, temputs, prototype, structFullName)
  }

  // This doesnt make a constructor, but its easy enough,
  def prototypeToAnonymousSubstruct(
      temputs: TemputsBox,
      interfaceRef2: InterfaceRef2,
      prototype: Prototype2):
  (StructRef2, FunctionHeader2) = {
    val functionStructRef = prototypeToAnonymousStruct(temputs, prototype)
    val functionStructType = Coord(Share, functionStructRef)

    val (anonymousSubstructRef, _) =
      makeAnonymousSubstruct(temputs, interfaceRef2, List(functionStructType))
    val anonymousSubstructType = Coord(Share, anonymousSubstructRef)

    // Now we make a function which constructs a functionStruct, then constructs a substruct.
    val constructor2 =
      Function2(
        FunctionHeader2(
          prototype.fullName
            .addStep(AnonymousSubstructName2(List(functionStructType)))
            .addStep(ConstructorName2(List())),
          false, false,
          List(),
          anonymousSubstructType,
          None),
        List(),
        Block2(
          List(
            Construct2(
              anonymousSubstructRef,
              anonymousSubstructType,
              List(
                Construct2(
                  functionStructRef,
                  Coord(Share, functionStructRef),
                  List()))))))
    temputs.declareFunctionSignature(constructor2.header.toSignature, None)
    temputs.declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
    temputs.addFunction(constructor2);

    vassert(temputs.exactDeclaredSignatureExists(constructor2.header.fullName, constructor2.header.toBanner.paramTypes))

    (anonymousSubstructRef, constructor2.header)
  }

//  // Makes a functor for the given prototype.
//  def functionToLambda(
//    outerEnv: IEnvironment,
//    temputs: TemputsBox,
//    header: FunctionHeader2):
//  StructRef2 = {
//    StructTemplarTemplateArgsLayer.functionToLambda(outerEnv, temputs, header)
//  }

  def getMemberCoords(temputs: TemputsBox, structRef: StructRef2): List[Coord] = {
    temputs.structDefsByRef(structRef).members.map(_.tyype).map({
      case ReferenceMemberType2(coord) => coord
      case AddressMemberType2(_) => {
        // At time of writing, the only one who calls this is the inferer, who wants to know so it
        // can match incoming arguments into a destructure. Can we even destructure things with
        // addressible members?
        vcurious()
      }
    })
  }

  def citizenIsFromTemplate(temputs: TemputsBox, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
    // this print is probably here because once we add namespaces to the syntax
    // this will false-positive for two interfaces with the same name but in different
    // namespaces.
    println("someday this is going to bite us")

    (citizen, template) match {
      case (InterfaceRef2(fullName), InterfaceTemplata(_, interfaceA)) => {
        fullName.last match {
          case CitizenName2(humanName, templateArgs) => humanName == interfaceA.name.name
          case _ => vimpl()
        }
      }
      case (StructRef2(fullName), StructTemplata(_, structA)) => {
        fullName.last match {
          case CitizenName2(humanName, templateArgs) => humanName == structA.name.name
          case _ => vimpl()
        }
      }
      case _ => (false)
    }
  }

//  def headerToIFunctionSubclass(
//    env: IEnvironment,
//    temputs: TemputsBox,
//    header: FunctionHeader2):
//  StructRef2 = {
//    val (paramType, returnType) =
//      header.toPrototype match {
//        case Prototype2(_, List(paramType), returnType) => (paramType, returnType)
//        case _ => vimpl("Only IFunction1 implemented")
//      }
//    val Some(InterfaceTemplata(ifunction1InterfaceEnv, ifunction1InterfaceA)) =
//      env.getNearestTemplataWithName("IFunction1", Set(TemplataLookupContext))
//
//    val lambdaStructRef = StructTemplar.functionToLambda(env, temputs, header)
//
//    val ifunction1InterfaceRef =
//      StructTemplar.getInterfaceRef(
//        ifunction1InterfaceEnv,
//        temputs,
//        ifunction1InterfaceA,
//        List(
//          MutabilityTemplata(Immutable),
//          CoordTemplata(paramType),
//          CoordTemplata(returnType)))
//
//    makeAnonymousSubstruct()
//  }

  def prototypeToAnonymousIFunctionSubstruct(
      env: IEnvironment,
      temputs: TemputsBox,
      prototype: Prototype2):
  (InterfaceRef2, StructRef2, FunctionHeader2) = {
    val Prototype2(_, List(paramType), returnType) = prototype

    val Some(ifunction1Templata@InterfaceTemplata(_, _)) =
      env.getNearestTemplataWithName(CodeTypeNameA("IFunction1"), Set(TemplataLookupContext))
    val ifunction1InterfaceRef =
      StructTemplar.getInterfaceRef(
        temputs,
        ifunction1Templata,
        List(
          MutabilityTemplata(Immutable),
          CoordTemplata(paramType),
          CoordTemplata(returnType)))

    val (elementDropFunctionAsIFunctionSubstructStructRef, constructorHeader) =
      StructTemplar.prototypeToAnonymousSubstruct(
        temputs, ifunction1InterfaceRef, prototype)

    (ifunction1InterfaceRef, elementDropFunctionAsIFunctionSubstructStructRef, constructorHeader)
  }

  def makeStructConstructor(
    temputs: TemputsBox,
    maybeConstructorOriginFunctionA: Option[FunctionA],
    structDef: StructDefinition2,
    constructorFullName: FullName2[IFunctionName2]):
  FunctionHeader2 = {
    StructTemplarCore.makeStructConstructor(
      temputs, maybeConstructorOriginFunctionA, structDef, constructorFullName)
  }
}
