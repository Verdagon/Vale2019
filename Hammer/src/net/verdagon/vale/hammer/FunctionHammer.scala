package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.VariableId2
import net.verdagon.vale.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.vale.templar.types.{FunctionT2, Raw}
import net.verdagon.vale.{vassert, vassertSome, vfail}

object FunctionHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def translateFunctions(hinputs: Hinputs, hamuts0: Hamuts, functions2: List[Function2]):
  (Hamuts, List[FunctionRef3]) = {
    functions2.foldLeft((hamuts0, List[FunctionRef3]()))({
      case ((hamuts1, previousFunctions3), function2) => {
        val (hamuts2, function3) = translateFunction(hinputs, hamuts1, function2)
        (hamuts2, function3 :: previousFunctions3)
      }
    })
  }

  private def translateFunction(hinputs: Hinputs, hamuts0: Hamuts, function2: Function2):
  (Hamuts, FunctionRef3) = {
    println("Translating function " + function2.header.fullName)
    hamuts0.functionRefs.get(function2.header.toPrototype) match {
      case Some(functionRef3) => (hamuts0, functionRef3)
      case None => {
        val Function2(
            header @ FunctionHeader2(humanName, lambdaNumber, isExtern, isUserFunction, params2, returnType2, _),
            locals,
            body) = function2;

        val (hamuts1, prototype3) = translatePrototype(hinputs, hamuts0, header.toPrototype);
        val temporaryFunctionRef3 = FunctionRef3(prototype3);
        val hamuts2 = hamuts1.forwardDeclareFunction(header.toPrototype, temporaryFunctionRef3)

        val locals0 = Locals(Map[VariableId2, VariableId3](), Set[VariableId3](), Map[VariableId3, Local]());
        val stackHeight = StackHeight(0, 0, 0)
        val (hamuts6, _, body3, maybeResultAccess) =
          BlockHammer.translateBlock(hinputs, hamuts2, locals0, stackHeight, body)
        val resultCoord = maybeResultAccess.map(_.expectedType).getOrElse(Reference3(Raw, Void3()))
        if (resultCoord != prototype3.returnType) {
          vfail()
        }

        val function3 = Function3(prototype3, header.getAbstractInterface != None, isExtern, isUserFunction, body3);
        val hamuts7 = hamuts6.addFunction(header.toPrototype, function3)

        (hamuts7, temporaryFunctionRef3)
      }
    }
  }

  def translatePrototypes(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototypes2: List[Prototype2]):
  (Hamuts, List[Prototype3]) = {
    prototypes2 match {
      case Nil => (hamuts0, Nil)
      case headPrototype2 :: tailPrototypes2 => {
        val (hamuts1, headPrototype3) = translatePrototype(hinputs, hamuts0, headPrototype2)
        val (hamuts2, tailPrototypes3) = translatePrototypes(hinputs, hamuts1, tailPrototypes2)
        (hamuts2, headPrototype3 :: tailPrototypes3)
      }
    }
  }

  def translatePrototype(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, Prototype3) = {
    val Prototype2(fullName2, FunctionT2(params2, returnType2)) = prototype2;
    val (hamuts1, paramsTypes3) = TypeHammer.translateReferences(hinputs, hamuts0, params2)
    val (hamuts2, returnType3) = TypeHammer.translateReference(hinputs, hamuts1, returnType2)
    val functionNumber = vassertSome(hinputs.functionIds.get(prototype2.toSignature))
    val (hamuts3, fullName3) = NameHammer.translateName(hinputs, hamuts2, fullName2)
    val prototype3 = Prototype3(functionNumber, fullName3, paramsTypes3, returnType3)
    (hamuts3, prototype3)
  }

  def translateFunctionRef(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, FunctionRef3) = {
    val (hamuts1, prototype3) = translatePrototype(hinputs, hamuts0, prototype2);
    val functionRef3 = FunctionRef3(prototype3);
    (hamuts1, functionRef3)
  }
}
