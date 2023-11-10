package objects

import types._
import scala.annotation.tailrec

case class TypeError(s: String) extends RuntimeException(s)

object TypeChecker {

  private type TEnvironment = List[TBind]

  def typeOf(e: ExprExt): Type = typeOf(e, Nil)

  def typeOf(e: ExprExt, nv: TEnvironment): Type = e match
    case TrueExt() | FalseExt() => BoolT()
    case NilExt(t) => ListT(t)
    case IdExt(a) => nv.find(_.name == a) match
      case Some(t) => t.ty
      case _ => throw TypeError("Element with this id does not exist")
    case NumExt(_) => NumT()
    case StringExt(_) => StringT() 
    case ListExt(t, l) if !l.exists(typeOf(_, nv) != t) => ListT(t)
    case TupleExt(l) => TupleT(l.map(typeOf(_, nv)))
    case ProjExt(n, e) => typeOf(e, nv) match
      case TupleT(l) if n >= 0 && l.size > n => l(n)
      case _ => throw TypeError("Cannot project a non-tuple value")
    case FdExt(l, e) => FunT(l.map(_.ty), typeOf(e, l.map(p => TBind(p.name, p.ty)) ++ nv))
    case RecLamExt(n, pt, rt, p, e) if typeOf(e, TBind(p, pt) :: TBind(n, FunT(List(pt), rt)) :: nv) == rt =>
      FunT(List(pt), rt)
    case LetExt(l, e) => typeOf(e, l.map(p => TBind(p.name, typeOf(p.value, nv))) ++ nv)
    case LetRecExt(l, e) => val env = l.map(p => TBind(p.name, p.ty)) ++ nv
      if (l.exists(p => typeOf(p.value, env) != p.ty)) throw TypeError("bruh")
      typeOf(e, env)
    case CondExt(l) => typeCheckCond(l, BoolT(), nv)
    case CondEExt(l, e) => typeCheckCond(l, typeOf(e, nv), nv)
    case IfExt(a, b, c) if typeOf(a, nv) == BoolT() => typeOf(b, nv)
      typeOf(c, nv)
    case SetExt(i, e) => nv.find(_.name == i) match
      case Some(t) if t.ty == typeOf(e, nv) => t.ty
      case _ => throw TypeError("Element with this id does not exist")
    // TODO: Do type checking on objects
    case BinOpExt(op, a, b) => (op, typeOf(a, nv), typeOf(b, nv)) match
      case ("+" | "*" | "-", NumT(), NumT()) => NumT()
      case ("num=" | "num<" | "num>", NumT(), NumT()) => BoolT()
      case ("and" | "or", BoolT(), BoolT()) => BoolT()
      case ("cons", t, ListT(lt)) if t.equals(lt) => ListT(t)
      case ("setbox", RefT(t), rt) if t == rt => rt
      case ("seq", _, rt) => rt
      case ("str=", StringT(), StringT()) => BoolT()
      case ("str++", StringT(), StringT()) => StringT()
      case _ => throw TypeError("Invalid binary op type")
    case UnOpExt(op, a) => (op, typeOf(a, nv)) match
      case ("-", NumT()) => NumT()
      case ("not", BoolT()) => BoolT()
      case ("head", ListT(t)) => t
      case ("tail", ListT(t)) => ListT(t)
      case ("is-nil", ListT(_)) => BoolT()
      case ("box", t) => RefT(t)
      case ("unbox", RefT(t)) => t
      case _ => throw TypeError("Invalid unary op type")
    case AppExt(f, args) => typeOf(f, nv) match
      case FunT(params, e) if params.size == args.size && params.zip(args).forall(t => t._1 == typeOf(t._2, nv)) => e
      case _ => throw TypeError("Cannot do application on a non-function value")
    case _ => throw TypeError("Invalid type")

  @tailrec
  private def typeCheckCond(l: List[(ExprExt, ExprExt)], e: Type, nv: TEnvironment): Type = l match
    case Nil => e
    case (a, b) :: t => typeOf(a, nv) match
      case BoolT() =>
        typeOf(b, nv)
        typeCheckCond(t, e, nv)
      case _ => throw TypeError("Condition must be of boolean type")
    case null => throw TypeError("Invalid cond statement")

}