package me.kerfume.jisp

sealed trait AST

sealed trait Value extends AST

case class Num(value: Long) extends Value
case class Str(value: String) extends Value
case class Symbol(value: String) extends Value

case class JList(values: List[Value])

// create by function collector
case class Defun(name: String, args: List[Symbol], body: JList)
