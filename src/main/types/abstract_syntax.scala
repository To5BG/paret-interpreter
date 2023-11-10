package types

sealed abstract class ExprExt

case class TrueExt() extends ExprExt

case class FalseExt() extends ExprExt

case class NumExt(num: Int) extends ExprExt

case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt

case class UnOpExt(s: String, e: ExprExt) extends ExprExt

case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt

case class NilExt(listTy: Type) extends ExprExt

case class ListExt(listTy: Type, es: List[ExprExt]) extends ExprExt

case class AppExt(f: ExprExt, args: List[ExprExt]) extends ExprExt

case class IdExt(c: String) extends ExprExt

case class FdExt(params: List[Param], body: ExprExt) extends ExprExt

case class LetExt(binds: List[LetBindExt], body: ExprExt) extends ExprExt

case class CondExt(cs: List[(ExprExt, ExprExt)]) extends ExprExt

case class CondEExt(cs: List[(ExprExt, ExprExt)], e: ExprExt) extends ExprExt

case class SetExt(id: String, e: ExprExt) extends ExprExt

case class TupleExt(l: List[ExprExt]) extends ExprExt

case class ProjExt(n: Int, e: ExprExt) extends ExprExt

case class RecLamExt(name: String, paramTy: Type, retTy: Type, param: String, body: ExprExt) extends ExprExt

case class LetRecExt(binds: List[LetRecBindExt], body: ExprExt) extends ExprExt

case class StringExt(str: String) extends ExprExt

case class ObjectExt(fields: List[FieldExt], methods: List[MethodExt]) extends ExprExt

case class ObjectDelExt(del: ExprExt, fields: List[FieldExt], methods: List[MethodExt]) extends ExprExt

case class FieldExt(name: String, value: ExprExt)

case class MethodExt(name: String, args: List[String], body: ExprExt)

case class MsgExt(recvr: ExprExt, msg: String, args: List[ExprExt]) extends ExprExt

case class DoSeqExt(expr: List[ExprExt]) extends ExprExt

case class LetBindExt(name: String, value: ExprExt)

case class LetRecBindExt(name: String, ty: Type, value: ExprExt)

object ExprExt {
  val binOps: Set[String] = Set("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons", "setbox",
    "seq", "str=", "str++")
  val unOps: Set[String] = Set("-", "not", "head", "tail", "is-nil", "is-list", "box", "unbox")
  val reserved: Set[String] = binOps ++ unOps ++ Set("list", "if", "else", "lambda", "let", "true",
    "false", "rec-lam", "tuple", "proj", "set", "letrec", ":", "->", "Num", "Bool", "List", "Tuple",
    "Ref", "object", "field", "method", "msg", "self", "do-seq", "msgobj!")
}