package me.kerfume.assembly

import DSL._

object BuildIn {
  val plus: Method = defm("plus")("J", "J")(L)(
    load(0, L),
    load(2, L),
    addN
  )
}
