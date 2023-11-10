package types

sealed abstract class SExpr

case class SList(list: List[SExpr]) extends SExpr

case class SNum(int: Int) extends SExpr

case class SSym(symbol: String) extends SExpr