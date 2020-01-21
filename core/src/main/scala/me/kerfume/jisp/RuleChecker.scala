package me.kerfume.jisp

import cats.data.ValidatedNec
import cats.syntax.validated._

object RuleChecker {
  def valid[L] = ().validNec[L]
  // list head is allowed only symbol or list
  def checkListHead(xs: List[JList]): ValidatedNec[InvalidListHead, Unit] = {
    def check(x: JList): ValidatedNec[InvalidListHead, Unit] = {
      if (x.values.isEmpty) ().validNec
      else {
        x.values.head match {
          case _: Symbol => valid
          case h: JList  => check(h)
          case invalid   => InvalidListHead(invalid).invalidNec
        }
      }
    }
    xs.traverse(check).void
  }
  // allowed only `t:type`
  def checkMetaSymbol(): Either[InvalidMetaSymbol, Unit] = ???

  // allowed defun in top level list
  def checkDefunSymbol(): Either[InvalidDefunSymbol, Unit] = ???

  case class InvalidListHead(invalidValue: Value)
  case class InvalidMetaSymbol(invalidSymbol: Symbol)
  case class InvalidDefunSymbol()
}
