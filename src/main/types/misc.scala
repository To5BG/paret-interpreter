package types

case class Pointer(name: String, location: Int)

case class Cell(location: Int, value: Value)

case class TBind(name: String, ty: Type)

case class Param(name: String, ty: Type)