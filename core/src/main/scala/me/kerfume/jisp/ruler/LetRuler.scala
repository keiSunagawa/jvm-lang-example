package me.kerfume.jisp.ruler

import me.kerfume.jisp.NgAST._
import Ruler._
import cats.syntax.validated._
import cats.syntax.foldable._

object LetRuler {
  def rule(
      xs: List[JList]
  ): RulerV[Unit] = {
    xs.traverse_ { x =>
      if (x.values.isEmpty) valid
      else {
        x.values.traverse_ {
          case nest: JList => jlistIsNonLet(nest)
          case _           => valid
        }
      }
    }
  }
  private def jlistIsNonLet(ys: JList): RulerV[Unit] = {
    ys.values.traverse_ {
      case s: Symbol if s.value == Let.symbol =>
        InvalidPlaceLetSymbol(ys).invalidNel
      case nest: JList => jlistIsNonLet(nest)
      case _           => valid
    }
  }
}
