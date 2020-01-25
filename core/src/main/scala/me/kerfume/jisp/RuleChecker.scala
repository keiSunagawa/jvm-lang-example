package me.kerfume.jisp

import cats.data.ValidatedNel
import cats.syntax.validated._
import cats.syntax.semigroup._
import org.atnos.eff._
import cats.data.NonEmptyList

object RuleChecker {
  type Error = NonEmptyList[RuleCheckerInvalid]
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  type CheckerResult[A] = ValidatedNel[RuleCheckerInvalid, A]
  def valid[L] = List(()).validNel[L]

  // list head is allowed only symbol or list
  // TODO headがSymbolに固定された型に変換してもよい?
  def checkListHead(
      xs: List[JList]
  ): CheckerResult[Unit] = {
    def check(x: JList): CheckerResult[Unit] = {
      if (x.values.isEmpty) ().validNel
      else {
        val nestValidated = x.values.collect {
          case nest: JList => check(nest)
        }.sequence
        val headValidated = x.values.head match {
          case _: Symbol => valid
          case _: JList  => valid
          case invalid   => InvalidListHead(invalid).invalidNel
        }
        (nestValidated |+| headValidated).void
      }
    }
    xs.traverse(check).void
  }
  // allowed only `t:type`
  def checkMetaSymbol(
      xs: List[JList]
  ): CheckerResult[Unit] = {
    def check(x: JList): CheckerResult[Unit] = {
      if (x.values.isEmpty) ().validNel
      else {
        val nestValidated = x.values.collect {
          case nest: JList => check(nest)
        }.sequence
        val htValidated = x.values match {
          case h :: t =>
            val hv = h match {
              case Symbol(sym) if sym.contains(":") =>
                val metaSymbol = sym.split(":").head
                if (metaSymbol == "t") valid
                else InvalidMetaSymbol(Symbol(sym)).invalidNel
              case _ => valid
            }
            val tv = t.traverse {
              case Symbol(sym) if sym.contains(":") =>
                InvalidMetaSymbol(Symbol(sym)).invalidNel
              case _ => ().valid
            }
            hv |+| tv
          case Nil => valid
        }
        (nestValidated |+| htValidated).void
      }
    }
    xs.traverse(check).void
  }

  // allowed defun in top level list
  def checkDefunSymbol(
      xs: List[JList]
  ): CheckerResult[Unit] = {
    val defumSym = "defun"
    def nonDefum(ys: JList): CheckerResult[Unit] = {
      ys.values.traverse {
        case Symbol(sym) if sym == defumSym =>
          InvalidDefunSymbol().invalidNel
        case nest: JList => nonDefum(nest)
        case _           => valid
      }.void
    }
    def check(x: JList): CheckerResult[Unit] = {
      if (x.values.isEmpty) ().validNel
      else {
        x.values
          .collect {
            case nest: JList => nonDefum(nest)
          }
          .sequence
          .void
      }
    }

    xs.traverse(check).void
  }

  // allowed let in top level list
  def checkLetSymbol(
      xs: List[JList]
  ): CheckerResult[Unit] = {
    val letSym = "let"
    def nonDefum(ys: JList): CheckerResult[Unit] = {
      ys.values.traverse {
        case Symbol(sym) if sym == letSym =>
          InvalidLetSymbol().invalidNel
        case nest: JList => nonDefum(nest)
        case _           => valid
      }.void
    }
    def check(x: JList): CheckerResult[Unit] = {
      if (x.values.isEmpty) ().validNel
      else {
        x.values
          .collect {
            case nest: JList => nonDefum(nest)
          }
          .sequence
          .void
      }
    }

    // TODO check (x let y) <- invalid code. and defun
    xs.traverse(check).void
  }

  sealed trait RuleCheckerInvalid
  case class InvalidListHead(invalidValue: Value) extends RuleCheckerInvalid
  case class InvalidMetaSymbol(invalidSymbol: Symbol) extends RuleCheckerInvalid
  case class InvalidDefunSymbol() extends RuleCheckerInvalid
  case class InvalidLetSymbol() extends RuleCheckerInvalid
}
