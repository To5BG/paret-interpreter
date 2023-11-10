import types.*
import objects.*
import org.scalatest.funsuite.AnyFunSuite

class InterpTest extends AnyFunSuite {

  test("Letrec") {
    assertResult(NumV(3)) {
      Interpreter.interp("(letrec ((x : Num 1) (y : Num 2)) (+ x y))")
    }
  }

  test("Doseq") {
    assertResult(NumV(4)) {
      Interpreter.interp("(let ((x 0)) (do-seq (set x 1) (set x (+ x x)) (set x (* x x)) x))")
    }
  }

  test("Object") {
    assertResult(PointerClosV(FdC(List("self", "msg"),
      IfC(EqStrC(IdC("msg"), StringC("get-x")), FdC(List(), IdC("x")),
        IfC(EqStrC(IdC("msg"), StringC("get-y")), FdC(List(), IdC("y")),
          IfC(EqStrC(IdC("msg"), StringC("set-x")), FdC(List("nx"), SetC("x", IdC("nx"))),
            IfC(EqStrC(IdC("msg"), StringC("set-y")), FdC(List("ny"), SetC("y", IdC("ny"))),
              UndefinedC()))))), List(Pointer("x", 0), Pointer("y", 1)))) {
      Interpreter.interp("(object ((field x 0) (field y 0)) " +
        "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) (method set-y (ny) (set y ny))))")
    }
  }

  test("No self") {
    intercept[ParseError] {
      Interpreter.interp("(let ((self 5)) true)")
    }
  }

  test("Message") {
    assertResult(NumV(42)) {
      Interpreter.interp("(let ((point (object ((field x 0) (field y 0)) " +
        "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) (method set-y (ny) (set y ny)))))) " +
        "(seq (msg point set-x 42) (msg point get-x)))")
    }
  }

  test("Encapsulation") {
    intercept[InterpError] {
      Interpreter.interp("(let ((point (object ((field x 0) (field y 0)) " +
        "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) " +
        "(method set-y (ny) (set y ny)))))) (msg point x))")
    }
  }

  test("Self reference") {
    assertResult(NumV(1)) {
      Interpreter.interp("(let " +
        "((point1 (object ((field val 1)) ((method get-value () val) (method set-value (nv) (set val nv)) " +
        "(method compare (p) (if (num< (msg p get-value) (msg self get-value)) p self))))) " +
        "(point2 (object ((field val 2)) ((method get-value () val) (method set-value (nv) (set val nv)) " +
        "(method compare (p) (if (num< (msg p get-value) (msg self get-value)) p self)))))) " +
        "(msg (msg point1 compare point2) get-value))")
    }
  }

  test("Message forwarding") {
    assertResult(NumV(3)) {
      Interpreter.interp("(letrec ((point2d : Num (object ((field x 0) (field y 0)) " +
        "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) (method set-y (ny) (set y ny))))) " +
        "(point3d : Num (object-del point2d ((field z 0)) ((method get-z () z) (method set-z (nz) (set z nz)))))) " +
        "(seq (msg point3d set-x 3) (msg point3d get-x)))")
    }
  }

  test("Late self-bind") {
    assertResult(ConsV(NumV(4), ConsV(NumV(2), NilV()))) {
      Interpreter.interp("(letrec ((vehicle-factory : Num (lambda () (object ((field position 1)) " +
        "((method speed-factor () 1) (method get-position () position) " +
        "(method move () (set position (* position (msg self speed-factor)))))))) " +
        "(car : Num (object-del (vehicle-factory) () ((method speed-factor () 4)))) " +
        "(bicycle : Num (object-del (vehicle-factory) () ((method speed-factor () 2))))) " +
        "(seq (msg car move) (seq (msg bicycle move) (cons (msg car get-position) " +
        "(cons (msg bicycle get-position) (nil : Num))))))")
    }
  }

}

