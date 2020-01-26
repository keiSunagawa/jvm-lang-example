package me.kerfume.jisp.ruler

import cats.data.NonEmptyList
import org.atnos.eff.|=
import me.kerfume.jisp.NgAST._
import cats.syntax.validated._
import cats.data.ValidatedNel

object Ruler {
  type Error = NonEmptyList[RulerInvalid]
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  type RulerV[A] = ValidatedNel[RulerInvalid, A]
  def valid[L] = List(()).validNel[L]

  sealed trait RulerInvalid
  case class InvalidListHead(invalid: JList) extends RulerInvalid
  case class EmptyListNotAllowed(invalid: JList) extends RulerInvalid
  case class InvalidPlaceDefunSymbol(invalid: JList) extends RulerInvalid
  case class InvalidPlaceLetSymbol(invalid: JList) extends RulerInvalid
}
