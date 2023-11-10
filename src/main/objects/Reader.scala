package objects

import types._
import scala.util.{Success, Try}

case class ReaderError(s: String) extends RuntimeException(s)

object Reader {

  def read(input: String): SExpr = input match
    case str if str.startsWith("(") && str.endsWith(")") =>
      SList(str.substring(1, str.length - 1).split(" ").toList.map(read))
    case str => Try(Integer.parseInt(str)) match
      case Success(i) => SNum(i)
      case _ => SSym(str)
    case null => throw ReaderError("")

}
