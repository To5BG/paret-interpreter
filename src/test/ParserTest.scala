import types.*
import objects.*
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("Number") {
    assertResult(NumExt(5)) {
      Parser.parse("5")
    }
  }

  test("Lambda") {
    assertResult(FdExt(List(Param("x", BoolT()), Param("y", BoolT())), TrueExt())) {
      Parser.parse("(lambda ((x : Bool) (y : Bool)) true)")
    }
  }

  test("Lambda #2") {
    assertResult(FdExt(List(Param("n", NumT())), TrueExt())) {
      Parser.parse("(lambda ((n : Num)) true)")
    }
  }

  test("Lambda #3") {
    assertResult(FdExt(List(), TrueExt())) {
      Parser.parse("(lambda () true)")
    }
  }

  test("Larger lambda") {
    assertResult(
      AppExt(
        AppExt(
          FdExt(List(Param("f", FunT(List(NumT()), NumT()))), FdExt(List(Param("x", NumT())),
            AppExt(IdExt("f"), List(IdExt("x"))))), List(FdExt(List(Param("y", NumT())),
            BinOpExt("*", IdExt("y"), IdExt("x"))))), List(NumExt(16)))) {
      Parser.parse("(((lambda ((f : ((Num) -> Num))) (lambda ((x : Num)) (f x))) " +
        "(lambda ((y : Num)) (* y x))) 16)")
    }
  }

  test("Binop") {
    assertResult(BinOpExt("+", BinOpExt("*", NumExt(1), NumExt(2)), BinOpExt("and", NumExt(3), NumExt(4)))) {
      Parser.parse("(+ (* 1 2) (and 3 4))")
    }
  }

  test("Cond") {
    assertResult(CondEExt(List((TrueExt(), FalseExt()), (TrueExt(), FalseExt())), FalseExt())) {
      Parser.parse("(cond (true false) (true false) (else false))")
    }
  }

  test("Let") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5))), TrueExt())) {
      Parser.parse("(let ((x 5)) true)")
    }
  }

  test("Let #2") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5))), NumExt(5))) {
      Parser.parse("(let ((x 5)) 5)")
    }
  }

  test("Let #3") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5)), LetBindExt("y", NumExt(4))), NumExt(6))) {
      Parser.parse("(let ((x 5) (y 4)) 6)")
    }
  }

  test("Empty let") {
    intercept[ParseError] {
      Parser.parse("(let () 5)")
    }
  }

  test("Recurring let") {
    intercept[ParseError] {
      Parser.parse("(let ((x 5) (x y)) 5)")
    }
  }

  test("Bad let argument") {
    intercept[ParseError] {
      Parser.parse("(let ((and 5)) 5)")
    }
  }

  test("Rec-Lam") {
    assertResult(RecLamExt("sum", NumT(), BoolT(), "n", TrueExt())) {
      Parser.parse("(rec-lam sum : ((Num) -> Bool) (n) true)")
    }
  }

  test("Larger rec-lam") {
    assertResult(RecLamExt("sum", NumT(), NumT(), "n", IfExt(BinOpExt("num=", IdExt("n"), NumExt(0)),
      NumExt(0), BinOpExt("+", IdExt("n"), AppExt(IdExt("sum"), List(BinOpExt("-", IdExt("n"), NumExt(1)))))))) {
      Parser.parse("(rec-lam sum : ((Num) -> Num) (n) (if (num= n 0) 0 (+ n (sum (- n 1)))))")
    }
  }

  test("List") {
    assertResult(UnOpExt("is-list", ListExt(BoolT(), List(TrueExt(), FalseExt())))) {
      Parser.parse("(is-list (list : Bool (true false)))")
    }
  }

  test("Nil") {
    assertResult(NilExt(NumT())) {
      Parser.parse("(nil : Num)")
    }
  }

  test("Empty") {
    intercept[ParseError] {
      Parser.parse("()")
    }
  }

  test("Empty cond") {
    intercept[ParseError] {
      Parser.parse("(cond)")
    }
  }

  test("Early else") {
    intercept[ParseError] {
      Parser.parse("(cond (else true))")
    }
  }

  test("Incomplete if") {
    intercept[ParseError] {
      Parser.parse("(if true true)")
    }
  }

  test("Branch-less if") {
    intercept[ParseError] {
      Parser.parse("(if true)")
    }
  }

  test("Object") {
    assertResult(ObjectExt(List(FieldExt("x", NumExt(0)), FieldExt("y", NumExt(0))),
      List(MethodExt("get-x", List(), IdExt("x")),
        MethodExt("get-y", List(), IdExt("y")),
        MethodExt("set-x", List("nx"), SetExt("x", IdExt("nx"))),
        MethodExt("set-y", List("ny"), SetExt("y", IdExt("ny")))))) {
      Parser.parse("(object ((field x 0) (field y 0)) " +
        "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) (method set-y (ny) (set y ny))))")
    }
  }

}