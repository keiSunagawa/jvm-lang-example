package me.kerfume.jisp.collector

import cats.instances.either._
import cats.instances.tuple._
import cats.Foldable
import cats.syntax.either._
import org.atnos.eff.|=
import me.kerfume.jisp.NgAST._

object DefunCollector {
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  def collect(
      xs: List[JList]
  ): Result[(List[JList], List[Defun])] = {
    xs.traverse { x =>
        x.values.head match {
          case s: Symbol if s.value == Defun.symbol =>
            toDefun(x).map { r =>
              // left statements acm, right defun acm
              Nil -> List(r)
            }
          case _ =>
            // left statements acm, right defun acm
            Right(List(x) -> Nil)
        }
      }
      .map(Foldable[List].fold(_))
  }

  private def toDefun(x: JList): Either[Error, Defun] = {
    x match {
      case Apply(
          defun,
          (name: Symbol) :: (args: JList) :: (body: Apply) :: Nil,
          info
          ) if defun.value == Defun.symbol =>
        for {
          validArgs <- args.values.traverse { a =>
            checkArg(a).toRight(InvalidDefunArg(a, x))
          }
        } yield Defun(name, validArgs, body, info)
      case _ => InvalidDefunSyntax(x).asLeft
    }
  }

  private def checkArg(arg: Value): Option[Symbol] = {
    arg match {
      case s: Symbol => Some(s)
      case _         => None
    }
  }

  sealed trait Error
  case class InvalidDefunSyntax(invalid: JList) extends Error
  case class InvalidDefunArg(invalid: Value, all: JList) extends Error
}
