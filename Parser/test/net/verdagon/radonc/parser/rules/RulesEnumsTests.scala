package net.verdagon.radonc.parser.rules

import net.verdagon.radonc.{parser, vfail}
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import org.scalatest.{FunSuite, Matchers}

class RulesEnumsTests extends FunSuite with Matchers {
  private def compile[T](parser: VParser.Parser[T], code: String): T = {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(expr, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        expr
      }
    }
  }
  private def compile[T](code: String): PatternPP = {
    compile(atomPattern, code)
  }

  private def checkFail[T](parser: VParser.Parser[T], code: String) = {
    VParser.parse(parser, "") match {
      case VParser.NoSuccess(_, _) =>
      case VParser.Success(_, rest) => {
        if (!rest.atEnd) {
          fail(rest.pos.longString)
        }
        fail()
      }
    }
  }

  test("Ownership") {
    compile(rulePR, "#X") shouldEqual TemplexPR(RunePRT("X"))
    compile(rulePR, "#X: Ownership") shouldEqual TypedPR(Some("X"),OwnershipTypePR)
    compile(rulePR, "#X = own") shouldEqual EqualsPR(TemplexPR(RunePRT("X")),TemplexPR(OwnershipPRT(OwnP)))
    compile(rulePR, "#X: Ownership = own|borrow") shouldEqual
        EqualsPR(
          TypedPR(Some("X"),OwnershipTypePR),
          OrPR(List(TemplexPR(OwnershipPRT(OwnP)), TemplexPR(OwnershipPRT(BorrowP)))))
    compile(rulePR, ":Ownership") shouldEqual TypedPR(None,OwnershipTypePR)
    compile(rulePR, "own") shouldEqual TemplexPR(OwnershipPRT(OwnP))
    compile(rulePR, ":Ownership = own|share") shouldEqual
        EqualsPR(
          TypedPR(None,OwnershipTypePR),
          OrPR(List(TemplexPR(OwnershipPRT(OwnP)), TemplexPR(OwnershipPRT(ShareP)))))
  }

  test("Mutability") {
    compile(rulePR, "#X") shouldEqual TemplexPR(RunePRT("X"))
    compile(rulePR, "#X: Mutability") shouldEqual TypedPR(Some("X"),MutabilityTypePR)
    compile(rulePR, "#X = mut") shouldEqual EqualsPR(TemplexPR(RunePRT("X")),TemplexPR(MutabilityPRT(MutableP)))
    compile(rulePR, "#X: Mutability = mut") shouldEqual
        EqualsPR(
          TypedPR(Some("X"),MutabilityTypePR),
          TemplexPR(MutabilityPRT(MutableP)))
    compile(rulePR, ":Mutability") shouldEqual TypedPR(None,MutabilityTypePR)
    compile(rulePR, "mut") shouldEqual TemplexPR(MutabilityPRT(MutableP))
    compile(rulePR, ":Mutability = mut|imm") shouldEqual
        EqualsPR(
          TypedPR(None,MutabilityTypePR),
          OrPR(List(TemplexPR(MutabilityPRT(MutableP)), TemplexPR(MutabilityPRT(ImmutableP)))))
  }

  test("Location") {
    compile(rulePR, "#X") shouldEqual TemplexPR(RunePRT("X"))
    compile(rulePR, "#X: Location") shouldEqual TypedPR(Some("X"),LocationTypePR)
    compile(rulePR, "#X = inl") shouldEqual EqualsPR(TemplexPR(RunePRT("X")),TemplexPR(LocationPRT(InlineP)))
    compile(rulePR, "#X: Location = inl") shouldEqual
        EqualsPR(
          TypedPR(Some("X"),LocationTypePR),
          TemplexPR(LocationPRT(InlineP)))
    compile(rulePR, ":Location") shouldEqual TypedPR(None,LocationTypePR)
    compile(rulePR, "inl") shouldEqual TemplexPR(LocationPRT(InlineP))
    compile(rulePR, ":Location = inl|yon") shouldEqual
        EqualsPR(
          TypedPR(None,LocationTypePR),
          OrPR(List(TemplexPR(LocationPRT(InlineP)), TemplexPR(LocationPRT(YonderP)))))
  }

  test("Permission") {
    compile(rulePR, "#X") shouldEqual TemplexPR(RunePRT("X"))
    compile(rulePR, "#X: Permission") shouldEqual TypedPR(Some("X"),PermissionTypePR)
    compile(rulePR, "#X = rw") shouldEqual EqualsPR(TemplexPR(RunePRT("X")),TemplexPR(PermissionPRT(ReadwriteP)))
    compile(rulePR, "#X: Permission = rw") shouldEqual
        EqualsPR(
          TypedPR(Some("X"),PermissionTypePR),
          TemplexPR(PermissionPRT(ReadwriteP)))
    compile(rulePR, ":Permission") shouldEqual TypedPR(None,PermissionTypePR)
    compile(rulePR, "rw") shouldEqual TemplexPR(PermissionPRT(ReadwriteP))
    compile(rulePR, ":Permission = xrw|rw|ro") shouldEqual
        EqualsPR(
          TypedPR(None,PermissionTypePR),
          OrPR(
            List(
              TemplexPR(PermissionPRT(ExclusiveReadwriteP)),
              TemplexPR(PermissionPRT(ReadwriteP)),
              TemplexPR(PermissionPRT(ReadonlyP)))))
  }

}
