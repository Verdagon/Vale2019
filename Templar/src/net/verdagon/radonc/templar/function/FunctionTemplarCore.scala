package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.templata.TemplataTemplar
import net.verdagon.radonc.{vassert, vassertSome, vcurious, vfail}

import scala.collection.immutable.{List, Set}

object FunctionTemplarCore {
  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def evaluateFunctionForHeader(
      innerEnv: FunctionEnvironment,
      temputs: TemputsBox,
      function1: FunctionA,
      params2: List[Parameter2]):
  (FunctionHeader2) = {

    println("Evaluating function " + innerEnv.fullName)

    val isDestructor =
      function1.name == CallTemplar.DESTRUCTOR_NAME && params2.head.tyype.ownership == Own;

    function1.body match {
      case CodeBodyA(body) => {
        val (fate2, header, body2) =
          BodyTemplar.declareAndEvaluateFunctionBody(
            innerEnv, temputs, BFunctionA(function1, function1.name, body), params2, isDestructor)

        // Funny story... let's say we're current instantiating a constructor,
        // for example MySome:T().
        // The constructor returns a MySome:T, which means when we do the above
        // evaluating of the function body, we stamp the MySome:T struct.
        // That ends up stamping the entire struct, including the constructor.
        // That's what we were originally here for, and evaluating the body above
        // just did it for us O_o
        // So, here we check to see if we accidentally already did it.

        // Get the variables by diffing the function environment.
        // Remember, the near env contains closure variables, which we
        // don't care about here. So find the difference between the near
        // env and our latest env.
        vassert(fate2.variables.startsWith(innerEnv.variables))
        val introducedLocals =
          fate2.variables
            .drop(innerEnv.variables.size)
            .collect({
              case x @ ReferenceLocalVariable2(_, _, _) => x
              case x @ AddressibleLocalVariable2(_, _, _) => x
            })

        temputs.lookupFunction(header.toSignature) match {
          case None => {
            val function2 = Function2(header, introducedLocals, body2);
            temputs.addFunction(function2)
            (function2.header)
          }
          case Some(function2) => {
            (function2.header)
          }
        }
      }
      case AbstractBodyA => {
        val maybeRetCoord =
          function1.maybeRetCoordRune match {
            case None => vfail("Need return type for abstract function!")
            case Some(r) => innerEnv.getNearestTemplataWithName(r, Set(TemplataLookupContext))
          }
        val retCoord =
          maybeRetCoord match {
            case None => vfail("wat")
            case Some(CoordTemplata(r)) => r
          }
        val header =
          makeInterfaceFunction(innerEnv, temputs, Some(function1), params2, retCoord)
        (header)
      }
      case ExternBodyA => {
        val maybeRetCoord =
          innerEnv.getNearestTemplataWithName(function1.maybeRetCoordRune.get, Set(TemplataLookupContext))
        val retCoord =
          maybeRetCoord match {
            case None => vfail("wat")
            case Some(CoordTemplata(r)) => r
          }
        val header =
          makeExternFunction(
            temputs,
            innerEnv.fullName,
            function1.isUserFunction,
            params2,
            retCoord,
            Some(function1))
        (header)
      }
      case GeneratedBodyA(generatorId) => {
        val signature2 = Signature2(innerEnv.fullName, params2.map(_.tyype));
        val maybeRetTemplata =
          function1.maybeRetCoordRune match {
            case None => (None)
            case Some(retCoordRune) => innerEnv.getNearestTemplataWithName(retCoordRune, Set(TemplataLookupContext))
          }
        val maybeRetCoord =
          maybeRetTemplata match {
            case None => (None)
            case Some(CoordTemplata(retCoord)) => {
              temputs.declareFunctionReturnType(signature2, retCoord)
              (Some(retCoord))
            }
            case _ => vfail("Must be a coord!")
          }

        // Funny story... let's say we're current instantiating a constructor,
        // for example MySome:T().
        // The constructor returns a MySome:T, which means when we do the above
        // evaluating of the function body, we stamp the MySome:T struct.
        // That ends up stamping the entire struct, including the constructor.
        // That's what we were originally here for, and evaluating the body above
        // just did it for us O_o
        // So, here we check to see if we accidentally already did it.
        println("doesnt this mean we have to do this in every single generated function?")

        temputs.lookupFunction(signature2) match {
          case Some(function2) => {
            (function2.header)
          }
          case None => {
            val generator = temputs.functionGeneratorByName(generatorId)
            val header =
              generator.generate(
                innerEnv, temputs, Some(function1), params2, maybeRetCoord)
            vassert(header.toSignature == signature2)
            (header)
          }
        }
      }
    }
  }

  def makeExternFunction(
      temputs: TemputsBox,
      fullName: FullName2,
      isUserFunction: Boolean,
      params2: List[Parameter2],
      returnType2: Coord,
      maybeOrigin: Option[FunctionA]):
  (FunctionHeader2) = {
    val header = FunctionHeader2(fullName, 0, true, isUserFunction, params2, returnType2, maybeOrigin)
    val argLookups =
      header.params.zipWithIndex.map({ case (param2, index) => ArgLookup2(index, param2.tyype) })
    val function2 =
      Function2(header, List(), Block2(List(ExternFunctionCall2(header.toPrototype, argLookups))))

    temputs.declareFunctionReturnType(header.toSignature, header.returnType)
    temputs.addFunction(function2)
    (header)
  }


  def makeInterfaceFunction(
    env: FunctionEnvironment,
    temputs: TemputsBox,
    origin: Option[FunctionA],
    params2: List[Parameter2],
    returnReferenceType2: Coord):
  (FunctionHeader2) = {
    vassert(params2.exists(_.virtuality == Some(Abstract2)))
    val header =
      FunctionHeader2(
        env.fullName,
        0,
        isExtern = false,
        isUserFunction = false,
        params2,
        returnReferenceType2,
        origin)
    val function2 =
      Function2(
        header,
        List(),
        Block2(List(InterfaceFunctionCall2(
          header.toBanner,
          Coord(Raw, header.toPrototype.functionType),
          header.returnType,
          header.params.zipWithIndex.map({ case (param2, index) => ArgLookup2(index, param2.tyype) })))))

      temputs
        .declareFunctionReturnType(header.toSignature, returnReferenceType2)
      temputs.addFunction(function2)
    vassert(temputs.exactDeclaredSignatureExists(env.fullName, header.paramTypes))
    (header)
  }

  def makeConstructor(
    temputs: TemputsBox,
    maybeOriginFunction1: Option[FunctionA],
    structDef: StructDefinition2):
  (FunctionHeader2) = {
    val constructorParams =
      structDef.members.map({
        case StructMember2(name, _, ReferenceMemberType2(reference)) => {
          Parameter2(name, None, reference)
        }
      })
    val constructorReturnOwnership = if (structDef.mutability == Mutable) Own else Share
    val constructorReturnType = Coord(constructorReturnOwnership, structDef.getRef)
    // not virtual because how could a constructor be virtual
    val constructor2 =
      Function2(
        FunctionHeader2(
          structDef.fullName,
          0,
          false, false,
          constructorParams,
          constructorReturnType,
          maybeOriginFunction1),
        List(),
        Block2(
          List(
            Construct2(
              structDef.getRef,
              Coord(if (structDef.mutability == Mutable) Own else Share, structDef.getRef),
              constructorParams.zipWithIndex.map({ case (p, index) => ArgLookup2(index, p.tyype) })))))

    // we cant make the destructor here because they might have a user defined one somewhere

      temputs
        .declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
      temputs.addFunction(constructor2);

    vassert(temputs.exactDeclaredSignatureExists(constructor2.header.fullName, constructor2.header.toBanner.paramTypes))

    (constructor2.header)
  }

  def makeImplDestructor(
    env: IEnvironment,
    temputs: TemputsBox,
    maybeOriginFunction1: Option[FunctionA],
    structDef2: StructDefinition2,
    interfaceRef2: InterfaceRef2):
  (FunctionHeader2) = {
    val ownership = if (structDef2.mutability == Mutable) Own else Share
    val structRef2 = structDef2.getRef
    val structType2 = Coord(ownership, structRef2)

    val structDestructor =
      DestructorTemplar.getCitizenDestructor(env, temputs, structType2)

    val destructor2 =
      Function2(
        FunctionHeader2(
          FullName2(
            List(
              NamePart2(
                CallTemplar.INTERFACE_DESTRUCTOR_NAME,
                Some(List(CoordTemplata(structType2), KindTemplata(interfaceRef2)))))),
          0,
          false, false,
          List(Parameter2("this", Some(Override2(interfaceRef2)), structType2)),
          Coord(Raw, Void2()),
          maybeOriginFunction1),
        List(),
        Block2(
          List(
            FunctionPointerCall2(
              FunctionLookup2(structDestructor),
              List(ArgLookup2(0, structType2))))))

    // If this fails, then the signature the FunctionTemplarMiddleLayer made for us doesn't
    // match what we just made
    vassert(temputs.exactDeclaredSignatureExists(destructor2.header.toSignature))

    // we cant make the destructor here because they might have a user defined one somewhere

      temputs
        .declareFunctionReturnType(destructor2.header.toSignature, destructor2.header.returnType)
      temputs.addFunction(destructor2);

    vassert(temputs.exactDeclaredSignatureExists(destructor2.header.toSignature))

    (destructor2.header)
  }
}
