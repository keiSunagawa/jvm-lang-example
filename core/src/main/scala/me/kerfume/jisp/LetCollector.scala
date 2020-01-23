package me.kerfume.jisp

import cats.syntax.either._
import cats.instances.either._

object LetCollector {
  def collect(xs: List[JList]): Either[InvalidLetSyntax, List[Statement]] = {
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

  case class InvalidLetSyntax()
}
