import types.*
import objects.*
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("Parse 5") {
    assertResult(NumExt(5)) {
      Parser.parse("5")
    }
  }

  test("Parse 6") {
    assertResult(FdExt(List(Param("x", BoolT()), Param("y", BoolT())), TrueExt())) {
      Parser.parse("(lambda (x y) true)")
    }
  }

  test("Parse 7") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5))), TrueExt())) {
      Parser.parse("(let ((x 5)) true)")
    }
  }

  test("Parse 8") {
    assertResult(
      AppExt(
        AppExt(
          FdExt(List("f"), FdExt(List("x"), AppExt(IdExt("f"), List(IdExt("x"))))),
          List(FdExt(List("y"), BinOpExt("*", IdExt("y"), IdExt("x"))))),
        List(NumExt(16)))) {
      Parser.parse("(((lambda (f) (lambda (x) (f x))) (lambda (y) (* y x))) 16)")
    }
  }

  test("Parse 9 (Z-combinator)") {
    assertResult(true) {
      print(Parser.parse("(lambda (f) ((lambda (y) (y y)) (lambda (z) (f (lambda (x) ((z z) x))))))"))
      true
    }
  }

  test("test 01") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5))), NumExt(5))) {
      Parser.parse("(let ((x 5)) 5)")
    }
  }

  test("test 02") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(5)), LetBindExt("y", NumExt(4))), NumExt(6))) {
      Parser.parse("(let ((x 5) (y 4)) 6)")
    }
  }

  test("test 03") {
    intercept[ParseError] {
      Parser.parse("(let () 5)")
    }
  }

  test("test 04") {
    intercept[ParseError] {
      Parser.parse("(let ((x 5) (x y)) 5)")
    }
  }

  test("test 05") {
    intercept[ParseError] {
      Parser.parse("(let ((and 5)) 5)")
    }
  }

  test("test 06") {
    assertResult(RecLamExt("sum", "n", IfExt(BinOpExt("num=", IdExt("n"), NumExt(0)),
      NumExt(0), BinOpExt("+", IdExt("n"), AppExt(IdExt("sum"), List(BinOpExt("-", IdExt("n"), NumExt(1)))))))) {
      Parser.parse("(rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1)))))")
    }
  }

  test("test 07") {
    assertResult(RecLamExt("sum", "n", TrueExt())) {
      Parser.parse("(rec-lam sum (n) true)")
    }
  }

  test("test 08") {
    assertResult(FdExt(List("n"), TrueExt())) {
      Parser.parse("(lambda (n) true)")
    }
  }

  test("test 09") {
    assertResult(FdExt(List(), TrueExt())) {
      Parser.parse("(lambda () true)")
    }
  }

  test("test 010") {
    assertResult(FdExt(List("n"), IdExt("y"))) {
      Parser.parse("(lambda (n) y)")
    }
  }

  test("test 011") {
    assertResult(FdExt(List("m", "n"), BinOpExt("+", IdExt("m"), IdExt("n")))) {
      Parser.parse("(lambda (m n) (+ m n))")
    }
  }

  test("test 012") {
    intercept[ParseError] {
      Parser.parse("(let () true)")
    }
  }

}