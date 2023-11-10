package objects

import types._

case class DesugarError(s: String) extends RuntimeException(s)

object Desugarer {

  def desugar(str: String): ExprC = desugar(Parser.parse(Reader.read(str)))

  def desugar(e: SExpr): ExprC = desugar(Parser.parse(e))

  def desugar(e: ExprExt): ExprC = e match
    case TrueExt() => TrueC()
    case FalseExt() => FalseC()
    case NilExt(_) | ListExt(_, Nil) => NilC()
    case IdExt(i) => IdC(i)
    case NumExt(a) => NumC(a)
    case ListExt(_, l) => l.foldRight(NilC(): ExprC)((a, acc) => ConsC(desugar(a), acc))
    case TupleExt(l) => TupleC(l.map(desugar))
    case ProjExt(n, e) => ProjC(n, desugar(e))
    case FdExt(l, e) => FdC(l.map(_.name), desugar(e))
    case RecLamExt(n, _, _, p, e) => AppC(Z_combinator, List(FdC(List(n), FdC(List(p), desugar(e)))))
    case LetExt(l, e) => AppC(FdC(l.map(_.name), desugar(e)), l.map(el => desugar(el.value)))
    case LetRecExt(l, e) => AppC(FdC(l.map(_.name), l.foldRight(desugar(e))
    ((a, acc) => SeqC(SetC(a.name, desugar(a.value)), acc))), l.map(_ => UninitializedC()))
    case CondExt(l) => l.foldRight(UndefinedC(): ExprC)((t, acc) => IfC(desugar(t._1), desugar(t._2), acc))
    case CondEExt(l, e) => l.foldRight(desugar(e))((t, acc) => IfC(desugar(t._1), desugar(t._2), acc))
    case IfExt(a, b, c) => IfC(desugar(a), desugar(b), desugar(c))
    case SetExt(i, e) => SetC(i, desugar(e))
    case BinOpExt(op, a, b) => op match
      case "+" => PlusC(desugar(a), desugar(b))
      case "*" => MultC(desugar(a), desugar(b))
      case "-" => PlusC(desugar(a), MultC(NumC(-1), desugar(b)))
      case "num=" => EqNumC(desugar(a), desugar(b))
      case "num<" => LtC(desugar(a), desugar(b))
      case "num>" => LtC(desugar(b), desugar(a))
      case "and" => IfC(desugar(a), desugar(b), FalseC())
      case "or" => IfC(desugar(a), TrueC(), desugar(b))
      case "cons" => ConsC(desugar(a), desugar(b))
      case "setbox" => SetboxC(desugar(a), desugar(b))
      case "seq" => SeqC(desugar(a), desugar(b))
      case _ => throw DesugarError("Invalid binary operation")
    case UnOpExt(op, a) => op match
      case "-" => MultC(NumC(-1), desugar(a))
      case "not" => IfC(desugar(a), FalseC(), TrueC())
      case "head" => HeadC(desugar(a))
      case "tail" => TailC(desugar(a))
      case "is-nil" => IsNilC(desugar(a))
      case "is-list" => IsListC(desugar(a))
      case "box" => BoxC(desugar(a))
      case "unbox" => UnboxC(desugar(a))
      case _ => throw DesugarError("Invalid unary operation")
    case AppExt(f, l) => AppC(desugar(f), l.map(desugar))
    case null => throw DesugarError("Invalid desugaring")

  private val Z_combinator = FdC(List("f"), AppC(
    FdC(List("y"), AppC(IdC("y"), List(IdC("y")))),
    List(FdC(List("z"),
      AppC(
        IdC("f"),
        List(FdC(List("x"),
          AppC(
            AppC(IdC("z"), List(IdC("z"))),
            List(IdC("x"))
          )
        ))
      )
    ))
  ))

}
