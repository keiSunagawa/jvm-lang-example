package me.kerfume.jisp.collector

import cats.syntax.either._
import cats.instances.either._
import org.atnos.eff.|=
import me.kerfume.jisp.NgAST._

object LetCollector {
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  def collect(xs: List[JList]): Result[List[Statement]] = {
    def toLet(x: JList): Either[InvalidLetSyntax, Let] = x match {
      case Apply(_, (name: Symbol) :: (body: Apply) :: Nil, info) =>
        Let(name, body, info).asRight
      case _ => InvalidLetSyntax(x).asLeft
    }
    xs.traverse {
      case x @ Apply(s, _, _) if s.value == Let.symbol => toLet(x)
      case x: Apply                                    => x.asRight
      case x                                           => InvalidLetSyntax(x).asLeft
    }
  }

  sealed trait Error
  case class InvalidLetSyntax(invalid: JList) extends Error

}
