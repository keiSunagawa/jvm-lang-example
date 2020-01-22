package me.kerfume.jisp

sealed trait AST

sealed trait Value extends AST

case class Num(value: Long) extends Value
case class Str(value: String) extends Value
case class Symbol(value: String) extends Value

case class JList(values: List[Value]) extends Value {
  def add(v: Value): JList = copy(values = values :+ v)
  def isEmpty: Boolean = values.isEmpty
}

// create by function collector
case class FArg(sym: Symbol, tpe: JispType)
case class Defun(name: Symbol, args: List[FArg], body: JList)

// create by let collector
case class Let(name: Symbol, body: JList)

sealed trait JispType
object JispType {
  case object Number extends JispType
  case object String extends JispType
  case object WillInfer extends JispType
  case object Void extends JispType

  case class FunctionType(argsType: List[JispType], retType: JispType)
}
