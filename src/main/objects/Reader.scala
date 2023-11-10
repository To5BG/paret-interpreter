package objects

import types.*

import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

case class ReaderError(s: String) extends RuntimeException(s)

object Reader {

  private var (excess, last) = (0, 0)

  def read(input: String): SExpr = input match
    case str if str.startsWith("(") && str.endsWith(")") =>
      SList(traverse(str.substring(1, str.length - 1)).map(read))
    case str => Try(Integer.parseInt(str)) match
      case Success(i) => SNum(i)
      case _ => SSym(str)
    case null => throw ReaderError("")

  private def traverse(input: String): List[String] =
    excess = 0
    last = 0
    var arr = ArrayBuffer[String]()
    for i <- 0 until input.length do
      if input.charAt(i) == ' ' && excess == 0 then
        arr += input.substring(last, i)
        last = i + 1
      if input.charAt(i) == '(' then excess += 1
      if input.charAt(i) == ')' then excess -= 1
    arr += input.substring(last, input.length)
    arr.toList

}
