package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal._
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.VariableId2
import net.verdagon.vale.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.vale.{vassert, vassertSome, vfail}

object FunctionHammer {

  def newId(nodesByLine: Vector[NodeH]) = Hammer.newId(nodesByLine)

  def translateFunctions(hinputs: Hinputs, hamuts0: Hamuts, functions2: List[Function2]):
  (Hamuts, List[FunctionRefH]) = {
    functions2.foldLeft((hamuts0, List[FunctionRefH]()))({
      case ((hamuts1, previousFunctionsH), function2) => {
        val (hamuts2, functionH) = translateFunction(hinputs, hamuts1, function2)
        (hamuts2, functionH :: previousFunctionsH)
      }
    })
  }

  private def translateFunction(hinputs: Hinputs, hamuts0: Hamuts, function2: Function2):
  (Hamuts, FunctionRefH) = {
    println("Translating function " + function2.header.fullName)
    hamuts0.functionRefs.get(function2.header.toPrototype) match {
      case Some(functionRefH) => (hamuts0, functionRefH)
      case None => {
        val Function2(
            header @ FunctionHeader2(humanName, lambdaNumber, isExtern, isUserFunction, params2, returnType2, _),
            locals,
            body) = function2;

        val (hamuts1, prototypeH) = translatePrototype(hinputs, hamuts0, header.toPrototype);
        val temporaryFunctionRefH = FunctionRefH(prototypeH);
        val hamuts2 = hamuts1.forwardDeclareFunction(header.toPrototype, temporaryFunctionRefH)

        val locals0 = Locals(Map[VariableId2, VariableIdH](), Set[VariableIdH](), Map[VariableIdH, Local]());
        val stackHeight = StackHeight(0, 0, 0)
        val (hamuts6, _, bodyH, maybeResultAccess) =
          BlockHammer.translateBlock(hinputs, hamuts2, locals0, stackHeight, body)
        val resultCoord = maybeResultAccess.map(_.expectedType).getOrElse(ReferenceH(m.Raw, VoidH()))
        if (resultCoord != prototypeH.returnType) {
          vfail()
        }

        val functionH = FunctionH(prototypeH, header.getAbstractInterface != None, isExtern, isUserFunction, bodyH);
        val hamuts7 = hamuts6.addFunction(header.toPrototype, functionH)

        (hamuts7, temporaryFunctionRefH)
      }
    }
  }

  def translatePrototypes(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototypes2: List[Prototype2]):
  (Hamuts, List[PrototypeH]) = {
    prototypes2 match {
      case Nil => (hamuts0, Nil)
      case headPrototype2 :: tailPrototypes2 => {
        val (hamuts1, headPrototypeH) = translatePrototype(hinputs, hamuts0, headPrototype2)
        val (hamuts2, tailPrototypesH) = translatePrototypes(hinputs, hamuts1, tailPrototypes2)
        (hamuts2, headPrototypeH :: tailPrototypesH)
      }
    }
  }

  def translatePrototype(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, PrototypeH) = {
    val Prototype2(fullName2, params2, returnType2) = prototype2;
    val (hamuts1, paramsTypesH) = TypeHammer.translateReferences(hinputs, hamuts0, params2)
    val (hamuts2, returnTypeH) = TypeHammer.translateReference(hinputs, hamuts1, returnType2)
    val (hamuts3, fullNameH) = NameHammer.translateName(hinputs, hamuts2, fullName2)
    val prototypeH = PrototypeH(fullNameH, paramsTypesH, returnTypeH)
    (hamuts3, prototypeH)
  }

  def translateFunctionRef(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, FunctionRefH) = {
    val (hamuts1, prototypeH) = translatePrototype(hinputs, hamuts0, prototype2);
    val functionRefH = FunctionRefH(prototypeH);
    (hamuts1, functionRefH)
  }
}
