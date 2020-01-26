package me.kerfume

import org.atnos.eff.Fx

object Eff {
  type +:[A[_], X] = Fx.prepend[A, X]
}
