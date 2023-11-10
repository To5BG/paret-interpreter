import objects._

@main def main(): Unit =
    println(Interpreter.interp("(let ((point (object ((field x 0) (field y 0)) " +
      "((method get-x () x) (method get-y () y) (method set-x (nx) (set x nx)) (method set-y (ny) (set y ny)))))) " +
      "(seq (msg point set-x 42) (msg point get-x)))"))
    println(SafeInterpreter.interp("(+ 5 3)"))
