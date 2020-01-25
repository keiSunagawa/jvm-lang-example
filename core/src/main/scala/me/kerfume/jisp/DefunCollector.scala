package me.kerfume.jisp

import cats.instances.either._
import cats.instances.tuple._
import cats.Foldable
import cats.syntax.either._
import org.atnos.eff.|=

object DefunCollector {
  type Result[A] = Either[Error, A]
  type _result[R] = Result |= R

  def collect(
      xs: List[JList]
  ): Result[(List[JList], List[Defun])] = {
    def checkArgs(arg: Value): Either[InvalidDefun, FArg] = {
      arg match {
        case JList(Symbol(t) :: Symbol(v) :: Nil) =>
          t match {
            case "t:num" => FArg(Symbol(v), JispType.Number).asRight
            case "t:str" => FArg(Symbol(v), JispType.String).asRight
            case _       => InvalidDefun().asLeft
          }
        case Symbol(v) => FArg(Symbol(v), JispType.WillInfer).asRight
        case _         => InvalidDefun().asLeft
      }
    }
    def toDefun(x: JList): Either[InvalidDefun, Defun] = {
      x.values match {
        case _ :: Symbol(name) :: JList(args) :: JList(body) :: Nil =>
          for {
            validArgs <- args.traverse(checkArgs)
          } yield Defun(Symbol(name), validArgs, JList(body))
        case _ => InvalidDefun().asLeft
      }
    }
    xs.traverse { x =>
        x.values.head match {
          case Symbol(sym) if sym == "defun" =>
            toDefun(x).map { r =>
              Nil -> List(r)
            }
          case _ => Right(List(x) -> Nil)
        }
      }
      .map(Foldable[List].fold(_))
  }

  sealed trait Error
  case class InvalidDefun() extends Error
}
