package me.kerfume.jisp.ruler

import me.kerfume.jisp.NgAST._
import Ruler._
import cats.syntax.validated._
import cats.syntax.apply._

object ApplyRuler {
  def rule(
      xs: List[JList]
  ): RulerV[List[JList]] = {
    xs.traverse {
      case vl: ValueList => rule0(vl)
      case ap: Apply     => ap.validNel
    }
  }
  private def rule0(x: ValueList): RulerV[JList] = {
    if (x.values.isEmpty) EmptyListNotAllowed(x).invalidNel
    else {
      val checkedValues = x.values.traverse {
        case vl: ValueList => rule0(vl)
        case a             => a.validNel
      }
      val headCheck = x.values match {
        case (h: Symbol) :: t => Apply(h, t, x.info).validNel
        case _                => InvalidListHead(x).invalidNel
      }
      (checkedValues, headCheck).mapN {
        case (jl, ap) =>
          ap.copy(params = jl.tail)
      }
    }
  }
}
