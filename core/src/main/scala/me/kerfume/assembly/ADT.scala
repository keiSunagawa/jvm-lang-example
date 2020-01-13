package me.kerfume.assembly

sealed trait ADT
sealed trait Command
case class RawCommand(mnemonic: String, operand: List[String]) extends Command
case class Store(i: Int) extends Command
case class Print(valIndex: Int) extends Command
case class Method(
    name: String,
    args: List[String],
    body: List[Command],
    ret: String
    // isStatic: Boolean # every true
)
case class Clazz(pkg: String, name: String, methods: List[Method])
