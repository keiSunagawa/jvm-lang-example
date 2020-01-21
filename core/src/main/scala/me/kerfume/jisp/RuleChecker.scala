package me.kerfume.jisp

import cats.data.ValidatedNec
import cats.syntax.validated._
import cats.syntax.semigroup._

object RuleChecker {
  def valid[L] = List(()).validNec[L]
  // list head is allowed only symbol or list
  def checkListHead(xs: List[JList]): ValidatedNec[InvalidListHead, Unit] = {
    def check(x: JList): ValidatedNec[InvalidListHead, Unit] = {
      if (x.values.isEmpty) ().validNec
      else {
        val nestValidated = x.values.collect {
          case nest: JList => check(nest)
        }.sequence
        val headValidated = x.values.head match {
          case _: Symbol => valid
          case _: JList  => valid
          case invalid   => InvalidListHead(invalid).invalidNec
        }
        (nestValidated |+| headValidated).void
      }
    }
    xs.traverse(check).void
  }
  // allowed only `t:type`
  def checkMetaSymbol(
      xs: List[JList]
  ): ValidatedNec[InvalidMetaSymbol, Unit] = {
    def check(x: JList): ValidatedNec[InvalidMetaSymbol, Unit] = {
      if (x.values.isEmpty) ().validNec
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
                else InvalidMetaSymbol(Symbol(sym)).invalidNec
              case _ => valid
            }
            val tv = t.traverse {
              case Symbol(sym) if sym.contains(":") =>
                InvalidMetaSymbol(Symbol(sym)).invalidNec
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
  ): ValidatedNec[InvalidDefunSymbol, Unit] = {
    val defumSym = "defun"
    def nonDefum(ys: JList): ValidatedNec[InvalidDefunSymbol, Unit] = {
      ys.values.traverse {
        case Symbol(sym) if sym == defumSym =>
          InvalidDefunSymbol().invalidNec
        case nest: JList => nonDefum(nest)
        case _           => valid
      }.void
    }
    def check(x: JList): ValidatedNec[InvalidDefunSymbol, Unit] = {
      if (x.values.isEmpty) ().validNec
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
  ): ValidatedNec[InvalidLetSymbol, Unit] = {
    val letSym = "let"
    def nonDefum(ys: JList): ValidatedNec[InvalidLetSymbol, Unit] = {
      ys.values.traverse {
        case Symbol(sym) if sym == letSym =>
          InvalidLetSymbol().invalidNec
        case nest: JList => nonDefum(nest)
        case _           => valid
      }.void
    }
    def check(x: JList): ValidatedNec[InvalidLetSymbol, Unit] = {
      if (x.values.isEmpty) ().validNec
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

  case class InvalidListHead(invalidValue: Value)
  case class InvalidMetaSymbol(invalidSymbol: Symbol)
  case class InvalidDefunSymbol()
  case class InvalidLetSymbol()
}
