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
case class Defun(name: Symbol, args: List[Symbol], body: JList)

// create by let collector
case class Let(name: Symbol, body: JList)
