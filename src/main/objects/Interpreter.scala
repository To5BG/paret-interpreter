package objects

import types._
import scala.annotation.tailrec

case class InterpError(s: String) extends RuntimeException(s)

object Interpreter {

  private type Store = List[Cell]
  private type PointerEnvironment = List[Pointer]

  def interp(str: String): Value = interp(Desugarer.desugar(Parser.parse(Reader.read(str))))

  def interp(e: SExpr): Value = interp(Desugarer.desugar(Parser.parse(e)))

  def interp(e: ExprExt): Value = interp(Desugarer.desugar(e))

  def interp(e: ExprC): Value = interp(e, List(), List())._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = e match
    case TrueC() => (BoolV(true), st1)
    case FalseC() => (BoolV(false), st1)
    case NilC() => (NilV(), st1)
    case IdC(a) => nv.find(_.name == a) match
      case Some(p) => st1.find(_.location == p.location) match
        case Some(c) => (c.value, st1)
        case None => throw InterpError("Variable is out-of-scope")
      case None => throw InterpError("Variable is undefined")
    case StringC(s) => (StringV(s), st1)
    case NumC(a) => (NumV(a), st1)
    case TupleC(l) => chainTuple(l, nv, st1, Nil)
    case ProjC(n, t) => interp(t, nv, st1) match
      case (TupleV(l), st2) if n < l.size && n >= 0 => (l(n), st2)
      case _ => throw InterpError("Could not project")
    case a: FdC => (PointerClosV(a, nv), st1)
    case ConsC(l, r) =>
      val t = interp(l, nv, st1)
      val t2 = interp(r, nv, t._2)
      (ConsV(t._1, t2._1), t2._2)
    case IfC(a, b, c) => interp(a, nv, st1) match
      case (BoolV(true), s) => interp(b, nv, s)
      case (BoolV(false), s) => interp(c, nv, s)
      case _ => throw InterpError("Cannot interpret conditional on non-boolean expressions")
    case SetC(v: String, b: ExprC) =>
      if (!nv.exists(_.name == v)) throw InterpError("Could not find variable")
      val t = interp(b, nv, st1)
      (t._1, update(v, nv, t._2, t._1))
    case PlusC(l, r) =>
      val t = checkArithm(l, r, nv, st1)
      (NumV(t._1 + t._2), t._3)
    case MultC(l, r) =>
      val t = checkArithm(l, r, nv, st1)
      (NumV(t._1 * t._2), t._3)
    case EqNumC(l, r) =>
      val t = checkArithm(l, r, nv, st1)
      (BoolV(t._1 == t._2), t._3)
    case LtC(l, r) =>
      val t = checkArithm(l, r, nv, st1)
      (BoolV(t._1 < t._2), t._3)
    case EqStrC(l, r) =>
      val t = checkString(l, r, nv, st1)
      (BoolV(t._1 == t._2), t._3)
    case ConcStrC(l, r) =>
      val t = checkString(l, r, nv, st1)
      (StringV(t._1 ++ t._2), t._3)
    case SetboxC(a, b) => interp(a, nv, st1) match
      case (BoxV(l), st2: Store) => val t = interp(b, nv, st2); (t._1, update(l, t._2, t._1))
      case _ => throw InterpError("Cannot unbox a non-boxed value")
    case SeqC(a, b) => interp(b, nv, interp(a, nv, st1)._2)
    case HeadC(e) => interp(e, nv, st1) match
      case (ConsV(e, _), s) => (e, s)
      case _ => throw InterpError("Cannot apply list operation to non-list expressions")
    case TailC(e) => interp(e, nv, st1) match
      case (ConsV(_, t), s) => (t, s)
      case _ => throw InterpError("Cannot apply list operation to non-list expressions")
    case IsNilC(e) => interp(e, nv, st1) match
      case (_: NilV, s) => (BoolV(true), s)
      case (_: ConsV, s) => (BoolV(false), s)
      case _ => throw InterpError("Cannot apply list operation to non-list expressions")
    case BoxC(v: ExprC) =>
      val bv: (Value, Store) = interp(v, nv, st1)
      val n = nextLoc(bv._2)
      (BoxV(n), Cell(n, bv._1) :: bv._2)
    case UnboxC(v: ExprC) => interp(v, nv, st1) match
      case (vv: BoxV, s: Store) => (fetch(vv.l, s), s)
      case _ => throw InterpError("Cannot unbox a non-boxed value")
    case AppC(f, args: List[ExprC]) => val (argloc, st) = chainStoreArguments(args, nv, st1, Nil)
      interp(f, nv, st) match
        case (PointerClosV(FdC(params: List[String], e), onv), ost) if params.size == argloc.size =>
          interp(e, params.zip(argloc).map(el => Pointer(el._1, el._2)) ++ onv, ost)
        case _ => throw InterpError("Could not interpret function application")
    case UninitializedC() => (UninitializedV(), st1)
    case UndefinedC() => throw InterpError("Undefined behavior")
    case _ => throw InterpError("Could not interpret expression")

  private def checkArithm(l: ExprC, r: ExprC, nv: PointerEnvironment, st: Store): (Int, Int, Store) =
    val av = interp(l, nv, st) match
      case (NumV(v), st1) => (v, st1);
      case _ => throw InterpError("Cannot apply arithmetic operations to non-numeric values")
    val bv = interp(r, nv, av._2) match
      case (NumV(v), st2) => (v, st2);
      case _ => throw InterpError("Cannot apply arithmetic operations to non-numeric values")
    (av._1, bv._1, bv._2)

  private def checkString(l: ExprC, r: ExprC, nv: PointerEnvironment, st: Store): (String, String, Store) = {
    val av = interp(l, nv, st) match {
      case (StringV(v), st1) => (v, st1);
      case _ => throw InterpError("Cannot apply string operations to non-character values")
    }
    val bv = interp(r, nv, av._2) match {
      case (StringV(v), st2) => (v, st2);
      case _ => throw InterpError("Cannot apply string operations to non-character values")
    }
    (av._1, bv._1, bv._2)
  }
  
  @tailrec
  private def chainStoreArguments(args: List[ExprC], nv: PointerEnvironment,
                                  s: Store, r: List[Int]): (List[Int], Store) = args match
    case Nil => (r.reverse, s)
    case a :: t => val (v, ss) = interp(a, nv, s)
      val n = nextLoc(ss)
      chainStoreArguments(t, nv, Cell(n, v) :: ss, n :: r)

  def update(x: String, env: PointerEnvironment, st: Store, v: Value): Store = env.find(_.name == x) match
    case Some(Pointer(_, l)) => update(l, st, v)
    case None => throw new RuntimeException()

  def update(loc: Int, st: Store, v: Value): Store = st.find(_.location == loc) match
    case Some(Cell(_, _)) => st.map(el => if (el.location == loc) Cell(loc, v) else el)
    case None => throw new RuntimeException()

  private def fetch(loc: Int, st: Store): Value = st.find(_.location == loc) match
    case Some(Cell(_, v)) => v
    case None => throw new RuntimeException()

  private def nextLoc(st: Store): Int = if (st.isEmpty) 0 else st.map(_.location).max + 1

  @tailrec
  private def chainTuple(l: List[ExprC], nv: PointerEnvironment, s: Store, r: List[Value]): (TupleV, Store) = l match
    case Nil => (TupleV(r), s)
    case a :: t => val i = interp(a, nv, s); chainTuple(t, nv, i._2, r :+ i._1);

}

object SafeInterpreter {

  def interp(str: String): Value =
    val expr = Parser.parse(Reader.read(str))
    TypeChecker.typeOf(expr)
    Interpreter.interp(Desugarer.desugar(expr))

  def interp(e: SExpr): Value =
    val expr = Parser.parse(e)
    TypeChecker.typeOf(expr)
    Interpreter.interp(Desugarer.desugar(expr))

  def interp(e: ExprExt): Value =
    TypeChecker.typeOf(e)
    Interpreter.interp(Desugarer.desugar(e))

}