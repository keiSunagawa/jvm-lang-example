package me.kerfume

import org.atnos.eff.Fx

object Eff {
  type +:[A[_], X] = Fx.append[Fx.fx1[A], X]
}
