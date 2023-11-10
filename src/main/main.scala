import objects._

@main def main(): Unit =
    println(Interpreter.interp("(+ 5 3)"))
    println(SafeInterpreter.interp("(+ 5 3)"))
