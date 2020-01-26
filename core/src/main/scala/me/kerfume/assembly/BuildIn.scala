package me.kerfume.assembly

import DSL._

object BuildIn {
  val plus: Method = defm("plus")("J", "J")(L)(
    load(0, L),
    load(2, L),
    addN
  )
  val printN: Method = defm("printN")("J")(Void)(
    RawCommand(
      "getstatic",
      "java/lang/System/out" :: "Ljava/io/PrintStream;" :: Nil
    ),
    load(0, L),
    RawCommand("invokevirtual", "java/io/PrintStream/println(J)V" :: Nil)
  )

  val all = Seq(plus, printN)
}
