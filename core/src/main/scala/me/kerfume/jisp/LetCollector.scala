package me.kerfume.jisp

import cats.syntax.either._
import cats.instances.either._
import org.atnos.eff.|=

object LetCollector {
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  def collect(xs: List[JList]): Result[List[Statement]] = {
    def toLet(x: JList): Either[InvalidLetSyntax, Let] = x.values match {
      case _ :: (name: Symbol) :: (body: JList) :: Nil =>
        Let(name, body).asRight
      case _ => InvalidLetSyntax().asLeft
    }
    xs.traverse {
      case x @ JList(Symbol(sym) :: _) if sym == "let" => toLet(x)
      case x                                           => Apply(x).asRight
    }
  }

  sealed trait Error
  case class InvalidLetSyntax() extends Error
}
