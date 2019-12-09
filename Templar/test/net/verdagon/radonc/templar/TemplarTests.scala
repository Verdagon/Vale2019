package net.verdagon.radonc.templar

import net.verdagon.radonc.parser.{Program0, VParser}
import net.verdagon.radonc.scout.{ProgramS, Scout}
import net.verdagon.radonc.templar.env.{ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types.{NamePart2, _}
import net.verdagon.radonc._
import net.verdagon.radonc.astronomer.{Astronomer, ProgramA}
import org.scalatest.{FunSuite, Matchers, _}

class TemplarTests extends FunSuite with Matchers {
  // TODO: pull all of the templar specific stuff out, the unit test-y stuff

  class Compilation(code: String, useCommonEnv: Boolean = true) {
    var parsedCache: Option[Program0] = None
    var scoutputCache: Option[ProgramS] = None
    var astroutsCache: Option[ProgramA] = None
    var temputsCache: Option[CompleteProgram2] = None

    def getParsed(): Program0 = {
      parsedCache match {
        case Some(parsed) => parsed
        case None => {
          val parsed = VParser.runParser(code)
          vassert(parsed != None) // runNamifier returns a None if it failed
          parsedCache = parsed
          parsed.get
        }
      }
    }

    def getScoutput(): ProgramS = {
      scoutputCache match {
        case Some(scoutput) => scoutput
        case None => {
          val scoutput =
            if (useCommonEnv) {
              Scout.runScout(getParsed())
            } else {
              Scout.scoutProgram(getParsed())
            }
          scoutputCache = Some(scoutput)
          scoutput
        }
      }
    }

    def getAstrouts(): ProgramA = {
      astroutsCache match {
        case Some(astrouts) => astrouts
        case None => {
          val astrouts =
            if (useCommonEnv) {
              Astronomer.runAstronomer(getScoutput())
            } else {
              Astronomer.runAstronomer(getScoutput())
            }
          astroutsCache = Some(astrouts)
          astrouts
        }
      }
    }

    def getTemputs(): CompleteProgram2 = {
      temputsCache match {
        case Some(temputs) => temputs
        case None => {

          val temputs =
            if (useCommonEnv) {
              Templar.runTemplar(getAstrouts())
            } else {
              Templar.evaluate(getAstrouts())
            }
          temputsCache = Some(temputs)
          temputs
        }
      }
    }
  }

  test("Simple program returning an int") {
    val compile = new Compilation("fn main(){3}")
    val temputs = compile.getTemputs()
    temputs.only({
      case FunctionHeader2(simpleName("main"),0, false, true,List(), Coord(Share, Int2()), _) => true
    })
    temputs.only({ case IntLiteral2(3) => true })
  }

  test("Hardcoding negative numbers") {
    val compile = new Compilation("fn main(){-3}")
    compile.getTemputs().only({ case IntLiteral2(-3) => true })
  }

  test("Taking an argument and returning it") {
    val compile = new Compilation("fn main(a:Int){a}")
    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").onlyOf(classOf[Parameter2]).tyype == Coord(Share, Int2())
    val lookup = temputs.lookupFunction("main").allOf(classOf[LocalLookup2]).head;
    lookup.localVariable.id.variableName shouldEqual "a"
    lookup.reference shouldEqual Coord(Share, Int2())
  }

  test("Tests adding two numbers") {
    val compile = new Compilation("fn main(){ +(2, 3) }")
    val temputs = compile.getTemputs()
    temputs.only({ case IntLiteral2(2) => true })
    temputs.only({ case IntLiteral2(3) => true })
    temputs.only({
      case FunctionPointerCall2(
        FunctionLookup2(functionName("+")),
        List(
          IntLiteral2(2),
          IntLiteral2(3))) =>
    })
  }

  test("Recursion") {
    val compile = new Compilation("fn main() Int{main()}")
    val temputs = compile.getTemputs()

    // Make sure it inferred the param type and return type correctly
    temputs.lookupFunction("main").header.returnType shouldEqual Coord(Share, Int2())
  }

  test("Simple lambda") {
    val compile = new Compilation("fn main(){{7}()}")
    val temputs = compile.getTemputs()

    // Make sure it inferred the param type and return type correctly
    temputs.lookupFunction("main:lam1").header.returnType shouldEqual Coord(Share, Int2())
    temputs.lookupFunction("main").header.returnType shouldEqual Coord(Share, Int2())
  }

  test("Lambda with one magic arg") {
    val compile = new Compilation("fn main(){{_}(3)}")
    val temputs = compile.getTemputs()

    // Make sure it inferred the param type and return type correctly
    temputs.lookupFunction("main:lam1")
        .only({ case Parameter2(_, None, Coord(Share, Int2())) => })

    temputs.lookupFunction("main:lam1").header.returnType shouldEqual
        Coord(Share, Int2())
  }


  // Test that the lambda's arg is the right type, and the name is right
  test("Lambda with a type specified param") {
    val compile = new Compilation("fn main(){{(a:Int) +(a,a)}(3)}");
    val temputs = compile.getTemputs()

    val lambda = temputs.lookupFunction("main:lam1");

    // Check that the param type is right
    lambda.only({ case Parameter2("a", None, Coord(Share, Int2())) => {} })
    // Check the name is right
    lambda.header match { case functionName("main:lam1") => }

    val main = temputs.lookupFunction("main");
    main.only({ case FunctionLookup2(functionName("main:lam1")) => })
  }

  test("Test overloads") {
    val compile = new Compilation(
      """
        |fn ~(a:Int, b:Int){+(a, b)}
        |fn ~(a:Str, b:Str){+(a, b)}
        |fn main(){3 ~ 3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Tests that we use the right overload, and both the overloads made it into temputs
    vassert(temputs.getAllUserFunctions.size == 3)
    temputs.lookupFunction("main").header.returnType shouldEqual
        Coord(Share, Int2())
  }

  test("Test templates") {
    val compile = new Compilation(
      """
        |fn ~:#T(a:#T, b:#T)#T{a}
        |fn main(){true ~ false; 2 ~ 2; = 3 ~ 3;}
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Tests that we reuse existing stamps
    vassert(temputs.getAllUserFunctions.size == 3)
  }

  test("Test mutating a local var") {
    val compile = new Compilation("fn main(){let mut a = 3; mut (a) = 4; }")
    val temputs = compile.getTemputs();
    temputs.only({ case Mutate2(LocalLookup2(ReferenceLocalVariable2(VariableId2(_, "a"), Varying, _), _), IntLiteral2(4)) => })
  }

  test("Test taking a callable param") {
    val compile = new Compilation(
      """
        |fn do(callable) {callable()}
        |fn main() {do({ 3 })}
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.functions.collect({ case x @ functionName("do") => x }).head.header.returnType shouldEqual Coord(Share, Int2())
  }

  test("Stamps an interface template via a function return") {
    val compile = new Compilation(TemplateSamples.stampingViaReturn)
    val temputs = compile.getTemputs()
  }
//
//  test("Constructor is stamped even without calling") {
//    val compile = new Compilation(
//      """
//        |struct MyStruct imm {}
//        |fn wot(b: *MyStruct) *Int { 9 }
//      """.stripMargin)
//    val temputs = compile.getTemputs()
//
//    temputs.lookupFunction("MyStruct")
//  }

  test("Reads a struct member") {
    val compile = new Compilation(
      """
        |struct MyStruct { a: *Int; }
        |fn main() { let ms = MyStruct(7); = ms.a; }
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Check the struct was made
    temputs.only({
      case StructDefinition2(
      simpleName("MyStruct"),
      Mutable,
      List(StructMember2("a", Final, ReferenceMemberType2(Coord(Share, Int2()))))) =>
    })
    // Check there's a constructor
    temputs.only({
      case FunctionHeader2(
      simpleName("MyStruct"),
      0,
      _,
      false,
      List(Parameter2("a", None, Coord(Share, Int2()))),
      Coord(Own, StructRef2(simpleName("MyStruct"))),
      _) =>
    })
    // Check that we call the constructor
    temputs.only({
      case FunctionPointerCall2(
      FunctionLookup2(Prototype2(simpleName("MyStruct"), _)),
      List(IntLiteral2(7))) =>
    })
  }

  test("Tests defining an interface and an implementing struct") {
    val compile = new Compilation(
      """
        |interface MyInterface { }
        |struct MyStruct { }
        |impl MyStruct for MyInterface;
      """.stripMargin)
    val temputs = compile.getTemputs()

    val interfaceDef =
      temputs.only({
        case id @ InterfaceDefinition2(simpleName("MyInterface"), Mutable) => id
      })

    val structDef =
      temputs.only({
        case sd @ StructDefinition2(simpleName("MyStruct"), Mutable, _) => sd
      })

    vassert(temputs.impls.exists(impl => {
      impl.struct == structDef.getRef && impl.interface == interfaceDef.getRef
    }))
  }

  test("Tests stamping an interface template from a function param") {
    val compile = new Compilation(
      """
        |interface MyOption:#T rules(#T: Ref) { }
        |fn main(a: MyOption:*Int) { }
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.interfaces match {
      case List(InterfaceDefinition2(FullName2(List(NamePart2("MyOption", Some(List(CoordTemplata(Coord(Share, Int2()))))))), Mutable)) =>
    }
    vassert(temputs.lookupFunction("main").header.params.head.tyype ==
        Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MyOption",Some(List(CoordTemplata(Coord(Share, Int2()))))))))))

    // Can't run it because there's nothing implementing that interface >_>
  }

  test("Tests stamping a struct and its implemented interface from a function param") {
    val compile = new Compilation(
      """
        |interface MyOption:#T imm rules(#T: Ref) { }
        |struct MySome:#T imm rules(#T: Ref) { value: #T; }
        |impl MySome:#T for MyOption:#T;
        |fn moo(a: MySome:*Int) { }
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.only({
      case InterfaceDefinition2(FullName2(List(NamePart2("MyOption", Some(List(CoordTemplata(Coord(Share, Int2()))))))), Immutable) =>
    })
    val struct = temputs.lookupStruct(StructRef2(FullName2(List(NamePart2("MySome", Some(List(CoordTemplata(Coord(Share, Int2())))))))));

    temputs.impls.find(impl => {
      impl.struct == struct.getRef && impl.interface == InterfaceRef2(FullName2(List(NamePart2("MyOption", Some(List(CoordTemplata(Coord(Share, Int2()))))))))
    }).get
  }

  test("Tests single expression and single statement functions' returns") {
    val compile = new Compilation(
      """
        |struct MyThing { value: *Int; }
        |fn moo() { MyThing(4) }
        |fn main() { moo(); }
      """.stripMargin)

    val temputs = compile.getTemputs()
    val moo = temputs.lookupFunction("moo")
    moo.header.returnType match {
      case Coord(Own,StructRef2(simpleName("MyThing"))) =>
    }
    val main = temputs.lookupFunction("main")
    main.header.returnType match {
      case Coord(Raw,Void2()) =>
    }
  }

  test("Tests calling a templated struct's constructor") {
    val compile = new Compilation(TemplateSamples.callingTemplatedConstructor)

    val temputs = compile.getTemputs()

    temputs.lookupStruct(StructRef2(FullName2(List(NamePart2("MySome", Some(List(CoordTemplata(Coord(Share, Int2())))))))));

    val constructor = temputs.lookupFunction("MySome")
    constructor.header match {
      case
        FunctionHeader2(
        simpleName("MySome"),
        0,
        false,
        _,
        _,
        Coord(Own, StructRef2(FullName2(List(NamePart2("MySome", Some(List(CoordTemplata(Coord(Share, Int2()))))))))),
        _) =>
    }

    temputs.lookupFunction("main").only({
      case FunctionLookup2(functionName("MySome")) =>
    })
  }

  test("Tests upcasting from a struct to an interface") {
    val compile = new Compilation(InheritanceSamples.upcasting)
    val temputs = compile.getTemputs()

    val main = temputs.lookupFunction("main")

    main.only({ case ReferenceLocalVariable2(VariableId2(_,"x"),Final,Coord(Own,InterfaceRef2(simpleName("MyInterface")))) => })

    val upcast = main.onlyOf(classOf[StructToInterfaceUpcast2])
    vassert(upcast.resultRegister.reference == Coord(Own, InterfaceRef2(simpleName("MyInterface"))))
    vassert(upcast.innerExpr.resultRegister.reference == Coord(Own, StructRef2(simpleName("MyStruct"))))
  }

  test("Tests calling a virtual function") {
    val compile = new Compilation(InheritanceSamples.calling)
    val temputs = compile.getTemputs()

    temputs.only({
      case up @ StructToInterfaceUpcast2(innerExpr, InterfaceRef2(simpleName("Car"))) => {
        innerExpr.resultRegister.only({
          case StructRef2(simpleName("Toyota")) =>
        })
        vassert(up.resultRegister.reference.referend == InterfaceRef2(simpleName("Car")))
      }
    })
  }

  test("Tests calling a virtual function through a borrow ref") {
    val compile = new Compilation(InheritanceSamples.callingThroughBorrow)
    val temputs = compile.getTemputs()

    val main = temputs.lookupFunction("main")
    main.only({
      case FunctionLookup2(Prototype2(simpleName("doCivicDance"),FunctionT2(List(Coord(Borrow,InterfaceRef2(simpleName("Car")))),Coord(Share,Int2())))) =>
    })
  }

  test("Tests calling a templated function with explicit template args") {
    // Tests putting MyOption:Int as the type of x.
    val compile = new Compilation(
      """
        |fn moo:#T () rules(#T: Ref) { }
        |
        |fn main() {
        |	moo:*Int();
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
  }

  test("Tests making a variable with a pattern") {
    // Tests putting MyOption:Int as the type of x.
    val compile = new Compilation(
      """
        |interface MyOption:#T rules(#T: Ref) { }
        |
        |struct MySome:#T rules(#T: Ref) {}
        |impl MySome:#T for MyOption:#T;
        |
        |fn doSomething(opt: MyOption:*Int) *Int {
        |  = 9;
        |}
        |
        |fn main() Int {
        |	let x: MyOption:*Int = MySome:*Int();
        |	= doSomething(x);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
  }

  test("Tests a linked list") {
    val compile = new Compilation(OrdinaryLinkedList.code)
    val temputs = compile.getTemputs()
  }

  test("Tests a templated linked list") {
    val compile = new Compilation(TemplatedLinkedList.code)
    val temputs = compile.getTemputs()
  }

  test("Tests calling an abstract function") {
    val compile = new Compilation(InheritanceSamples.callingAbstract)
    val temputs = compile.getTemputs()

    temputs.functions.collectFirst({
      case Function2(header @ functionName("doThing"), _, _) if header.getAbstractInterface != None => true
    }).get
  }

  test("Tests a foreach for a linked list") {
    val compile = new Compilation(ForeachLinkedList.code)
    val temputs = compile.getTemputs()

    val main = temputs.lookupFunction("main")
    main.only({
      case f @ FunctionLookup2(functionName("forEach")) => f
    })
  }

  // Make sure a ListNode struct made it out
  test("Templated imm struct") {
    val compile = new Compilation(
      """
        |struct ListNode:#T imm rules(#T: Ref) {
        |  tail: ListNode:#T;
        |}
        |fn main(a: ListNode:Int) {}
      """.stripMargin)
    val temputs = compile.getTemputs()
  }


  test("Test Array of StructTemplata") {
    val compile = new Compilation(
      """
        |struct Vec2 imm {
        |  x: Float;
        |  y: Float;
        |}
        |struct Pattern imm {
        |  patternTiles: __Array:(imm, Vec2);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
  }

  test("Test array length") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let a = __Array:(mut, Int)(11, {_});
        |  = len(&a);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
  }

  test("Test return") {
    val compile = new Compilation(
      """
        |fn main() {
        |  ret 7;
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.only({ case Return2(_) => })
  }

  test("Test return from inside if") {
    val compile = new Compilation(
      """
        |fn main() {
        |  if {true} {
        |    ret 7;
        |  } else {
        |    ret 9;
        |  }
        |  = panic();
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.all({ case Return2(_) => }).size shouldEqual 2
    main.only({ case IntLiteral2(7) => })
    main.only({ case IntLiteral2(9) => })
  }

  test("Test return from inside if destroys locals") {
    val compile = new Compilation(
      """struct Marine { hp: Int; }
        |fn main() {
        |  let m = Marine(5);
        |  let x =
        |    if {true} {
        |      ret 7;
        |    } else {
        |      = m.hp;
        |    };
        |  = x;
        |}
        |""".stripMargin)
    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    val destructorCalls =
      main.all({ case fpc @ FunctionPointerCall2(FunctionLookup2(Prototype2(FullName2(List(NamePart2("destructor",Some(List(CoordTemplata(Coord(Own,StructRef2(simpleName("Marine"))))))))),_)),_) => fpc })
    destructorCalls.size shouldEqual 2
  }

  test("Test complex interface") {
    val compile = new Compilation(TemplatedInterface.code)
    val temputs = compile.getTemputs()
  }

  test("Local-mut upcasts") {
    val compile = new Compilation(
      """
        |interface IOption:#T rules(#T: Ref) { }
        |struct Some:#T rules(#T: Ref) { value: #T; }
        |impl Some:#T for IOption:#T;
        |struct None:#T rules(#T: Ref) { }
        |impl None:#T for IOption:#T;
        |
        |fn main() {
        |  let m: IOption:Int = None:Int();
        |  mut (m) = Some(6);
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.only({
      case Mutate2(_, StructToInterfaceUpcast2(_, _)) =>
    })
  }

  test("Expr-mut upcasts") {
    val compile = new Compilation(
      """
        |interface IOption:#T rules(#T: Ref) { }
        |struct Some:#T rules(#T: Ref) { value: #T; }
        |impl Some:#T for IOption:#T;
        |struct None:#T rules(#T: Ref) { }
        |impl None:#T for IOption:#T;
        |
        |struct Marine {
        |  weapon: IOption:Int;
        |}
        |fn main() {
        |  let m = Marine(None:Int());
        |  mut (m.weapon) = Some(6);
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.only({
      case Mutate2(_, StructToInterfaceUpcast2(_, _)) =>
    })
  }

  test("Lambda inside template") {
    // This originally didn't work because both helperFunc<Int> and helperFunc<Str>
    // made a closure struct called helperFunc:lam1, which collided.
    // This is what spurred namespace support.

    val compile = new Compilation(
      """fn helperFunc(x: #T) {
        |  { print(x); }();
        |}
        |fn main() {
        |  helperFunc(4);
        |  helperFunc("bork");
        |}
        |""".stripMargin)
    val temputs = compile.getTemputs()
  }


  test("Lambda inside different function with same name") {
    // This originally didn't work because both helperFunc(:Int) and helperFunc(:Str)
    // made a closure struct called helperFunc:lam1, which collided.
    // We need to disambiguate by parameters, not just template args.

    val compile = new Compilation(
      """fn helperFunc(x: Int) {
        |  { print(x); }();
        |}
        |fn helperFunc(x: Str) {
        |  { print(x); }();
        |}
        |fn main() {
        |  helperFunc(4);
        |  helperFunc("bork");
        |}
        |""".stripMargin)
    val temputs = compile.getTemputs()
  }
}
