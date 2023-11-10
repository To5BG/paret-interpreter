import types.*
import objects.*
import org.scalatest.funsuite.AnyFunSuite

class TypeCheckerTest extends AnyFunSuite {

  test("Letrec types") {
    assertResult(NumT()) {
      TypeChecker.typeOf(Parser.parse("(letrec ((x : Num 1) (y : Num 2)) (+ x y))"))
    }
  }

  test("Invalid letrec types") {
    intercept[TypeError] {
      TypeChecker.typeOf(Parser.parse("(letrec ((x : ((Num) -> Num) 1) (y : ((Num) -> Num) 2)) (+ x y))"))
    }
  }

  test("Invalid letrec types #2") {
    intercept[TypeError] {
      TypeChecker.typeOf(Parser.parse("(letrec ((x : ((Num) -> Num) 1) (y : Num 2)) (+ x y))"))
    }
  }

}
