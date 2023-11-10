package objects

import types._

case class ParseError(s: String) extends RuntimeException(s)

object Parser {

  private var allowSelf: Boolean = false

  def parse(str: String): ExprExt = parse(Reader.read(str))

  def parse(sexpr: SExpr): ExprExt = sexpr match
    case SSym("true") => TrueExt()
    case SSym("false") => FalseExt()
    case SSym(s) if (allowSelf && s == "self") || !ExprExt.reserved.contains(s) => IdExt(s)
    case SNum(a) => NumExt(a)
    case SList(SSym("nil") :: SSym(":") :: ty :: Nil) => NilExt(resolveType(ty))
    case SList(SSym("list") :: SSym(":") :: ty :: SList(t) :: Nil) => ListExt(resolveType(ty), t.map(parse))
    case SList(SSym("tuple") :: t) if t.size > 1 => TupleExt(t.map(parse))
    case SList(SSym("proj") :: SNum(i) :: s :: Nil) => ProjExt(i, parse(s))
    case SList(SSym("lambda") :: SList(p) :: e :: Nil) => FdExt(validIdList(p), parse(e))
    case SList(SSym("rec-lam") :: SSym(s) :: SSym(":") :: ty :: SList(SSym(ss) :: Nil) :: e :: Nil) =>
      resolveType(ty) match
        case FunT(p, ret) if p.size == 1 && !ExprExt.reserved.contains(s) => RecLamExt(s, p.head, ret, ss, parse(e))
        case _ => throw ParseError("")
    case SList(SSym("let") :: SList(l) :: e :: Nil) if l.nonEmpty => LetExt(valBinds(l), parse(e))
    case SList(SSym("letrec") :: SList(l) :: e :: Nil) if l.nonEmpty => LetRecExt(valRecBinds(l), parse(e))
    case SList(SSym("cond") :: t) => t match
      case b :+ SList(SSym("else") :: e :: Nil) if b.nonEmpty => CondEExt(validConds(b), parse(e))
      case b if b.nonEmpty => CondExt(validConds(b))
      case _ => throw ParseError("")
    case SList(SSym("if") :: a :: b :: c :: Nil) => IfExt(parse(a), parse(b), parse(c))
    case SList(SSym("set") :: SSym(id) :: s :: Nil) if !ExprExt.reserved.contains(id) => SetExt(id, parse(s))
    case SList(SSym("do-seq") :: l) if l.nonEmpty => DoSeqExt(l.map(parse))
    case SList(SSym("object") :: SList(p) :: SList(m) :: Nil) => parseObject(p, m, null)
    case SList(SSym("object-del") :: expr :: SList(p) :: SList(m) :: Nil) => parseObject(p, m, expr)
    case SList(SSym("msg") :: expr :: SSym(msg) :: t) => MsgExt(parse(expr), msg, t.map(parse))
    case SList(SSym(s) :: a :: b :: Nil) if ExprExt.binOps.contains(s) => BinOpExt(s, parse(a), parse(b))
    case SList(SSym(s) :: a :: Nil) if ExprExt.unOps.contains(s) => UnOpExt(s, parse(a))
    case SList(e :: l) => AppExt(parse(e), l.map(parse))
    case _ => throw ParseError("")

  private def validIdList(l: List[SExpr]): List[Param] =
    val list = l.map {
      case SList(SSym(s) :: SSym(":") :: ty :: Nil) if !ExprExt.reserved.contains(s) => Param(s, resolveType(ty))
      case _ => throw ParseError("")
    }
    if (list.distinct.size == l.size) list
    else throw ParseError("")

  private def valBinds(l: List[SExpr]): List[LetBindExt] =
    val list = l.map {
      case SList(SSym(s) :: e :: Nil) if !ExprExt.reserved.contains(s) => LetBindExt(s, parse(e))
      case _ => throw ParseError("")
    }
    if (list.map(_.name).distinct.size == l.size) list
    else throw ParseError("")

  private def valRecBinds(l: List[SExpr]): List[LetRecBindExt] =
    val list = l.map {
      case SList(SSym(s) :: SSym(":") :: ty :: e :: Nil) if !ExprExt.reserved.contains(s) =>
        LetRecBindExt(s, resolveType(ty), parse(e))
      case _ => throw ParseError("")
    }
    if (list.map(_.name).distinct.size == l.size) list
    else throw ParseError("")

  private def validConds(l: List[SExpr]): List[(ExprExt, ExprExt)] =
    l.map {
      case SList(a :: b :: Nil) => (parse(a), parse(b))
      case _ => throw ParseError("")
    }

  private def parseObject(params: List[SExpr], methods: List[SExpr], e: SExpr): ExprExt =
    try {
      val pParams = params.map {
        case SList(SSym("field") :: SSym(s) :: expr :: Nil) if !ExprExt.reserved.contains(s) => FieldExt(s, parse(expr))
        case _ => throw ParseError("")
      }
      val pMethods = methods.map {
        case SList(SSym("method") :: SSym(s) :: SList(l) :: expr :: Nil) if !ExprExt.reserved.contains(s) =>
          allowSelf = true
          MethodExt(s, l.map {
            case SSym(p) if !ExprExt.reserved.contains(p) => p
            case _ => throw ParseError("")
          }, parse(expr))
        case _ => throw ParseError("")
      }
      if (e != null) ObjectDelExt(parse(e), pParams, pMethods) else ObjectExt(pParams, pMethods)
    } finally allowSelf = false

  private def resolveType(list: SExpr): Type = list match
    case SSym("Num") => NumT()
    case SSym("Bool") => BoolT()
    case SList(SList(par) :: SSym("->") :: ret :: Nil) => FunT(par.map(resolveType), resolveType(ret))
    case SList(SSym("List") :: s :: Nil) => ListT(resolveType(s))
    case SList(SSym("Tuple") :: s) => TupleT(s.map(resolveType))
    case SList(SSym("Ref") :: s :: Nil) => RefT(resolveType(s))
    case _ => throw ParseError("")

}