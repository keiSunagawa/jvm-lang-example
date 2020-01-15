package me.kerfume.assembly

sealed trait ADT
sealed trait Command
case class RawCommand(nemonic: String, operand: List[String]) extends Command
case class Store(i: Int, tpe: Tpe) extends Command
case class Load(i: Int, tpe: Tpe) extends Command
case class NConst(x: Long) extends Command
case class SConst(x: String) extends Command
case class Print(valIndex: Int, tpe: Tpe) extends Command
case class Method(
    name: String,
    args: List[String],
    body: List[Command],
    ret: String
    // isStatic: Boolean # every true
)
case class Clazz(pkg: String, name: String, methods: List[Method])

sealed trait Tpe
//case object I extends Tpe // Int
case object L extends Tpe // Long
// case object B extends Tpe // Byte
// case object F extends Tpe // Fload
// case object D extends Tpe // Double
case class array(elemTpe: Tpe) extends Tpe
case class ref(value: String) extends Tpe // class instance
object Tpe {
  val String = "Ljava/lang/String;"
  val Object = "Ljava/lang/Object;"
}
