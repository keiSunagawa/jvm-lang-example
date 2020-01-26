package me.kerfume.jisp

sealed trait AST

sealed trait Value extends AST
//sealed trait List

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
sealed trait Statement
case class Apply(body: JList) extends Statement
case class Let(name: Symbol, body: JList) extends Statement
object Let {
  val str = "let"
}

sealed trait JispType
object JispType {
  // TODO rename function引数以外にもvoid以外の型の集合として使っている
  sealed trait FunArgType extends JispType
  case object Number extends FunArgType
  case object String extends FunArgType
  case object Void extends JispType

  case class FunctionType(
      argsType: List[JispType.FunArgType],
      retType: JispType
  )

  def toNonVoid(tpe: JispType): Option[FunArgType] =
    PartialFunction.condOpt(tpe) {
      case a: FunArgType => a
    }
}

sealed trait NgAST {
  def info: CodeInfo
}
object NgAST {
  sealed trait Atom extends AST
  sealed trait Value extends AST

  sealed trait Statement

  /** e.g. 1 */
  case class Num(value: Long, info: CodeInfo) extends Atom with Value

  /** e.g. "foo" */
  case class Str(value: String, info: CodeInfo) extends Atom with Value

  /** e.g. add */
  case class Symbol(value: String, info: CodeInfo) extends Atom with Value

  sealed trait JList extends Value {
    def values: List[Value]
  }

  /** e.g. (1 a "a") */
  case class ValueList(values: List[Value], info: CodeInfo) extends JList {
    def add(v: Value): ValueList = copy(values = values :+ v)
    def isEmpty: Boolean = values.isEmpty
  }

  /** e.g. (f a 1) */
  case class Apply(f: Symbol, params: List[Value], info: CodeInfo)
      extends JList
      with Statement {
    def values: List[Value] = f :: params
  }

  sealed trait Define extends NgAST

  /** e.g. (defun twice (x y) (add (add x y) (add x y))) */
  case class Defun(
      name: Symbol,
      args: List[Symbol],
      body: Apply,
      info: CodeInfo
  ) extends Define
  object Defun {
    val symbol = "defun"
  }

  /** e.g. (let x (twice 1 2)) */
  case class Let(name: Symbol, body: Apply, info: CodeInfo)
      extends Define
      with Statement
  object Let {
    val symbol = "let"
  }

  object TypeAnnon {
    val prefix = "t"
    def check(sym: Symbol): Boolean = {
      sym.value.split(":").headOption.forall(_ == prefix)
    }
  }
}
case class CodeInfo(line: Int, start: Int, end: Int, text: String) {
  override def toString = s"line: $line, start: $start, end: $end"
}
