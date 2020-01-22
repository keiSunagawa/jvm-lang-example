package me.kerfume.jisp

import cats.syntax.either._
import cats.instances.either._
import me.kerfume.jisp.JispType.WillInfer

class TypeInfer(val functionTypeMap: Map[String, JispType.FunctionType]) {
  def infer(defun: Defun): Either[TypeMisMatch, JispType.FunctionType] = {
    var variableTypeMap: Map[Symbol, JispType] = Map.empty

    // initialize
    defun.args.foreach { a =>
      variableTypeMap = variableTypeMap + (a.sym -> a.tpe)
    }

    def applyCheck(exp: JList): Either[TypeMisMatch, JispType] = {
      exp.values match {
        case Nil => JispType.Void.asRight
        case Symbol(sym) :: other =>
          for {
            params <- other.traverse {
              case xs: JList => applyCheck(xs).map(Constant)
              case _: Num    => Constant(JispType.Number).asRight
              case _: Str    => Constant(JispType.String).asRight
              case x: Symbol =>
                variableTypeMap
                  .get(x)
                  .map(Variable(x, _))
                  .toRight(VaeiableNotFound())
            }
            f <- functionTypeMap.get(sym).toRight(FunctionNotFound())
            _ <- Either.cond(
              f.argsType.size == params.size,
              ().asRight,
              ToManyArgs()
            )
            _ <- params.zip(f.argsType).traverse {
              case (p, a) =>
                p match {
                  case Constant(t) if t == a => ().asRight
                  case Variable(sym, WillInfer) =>
                    variableTypeMap = variableTypeMap + (sym -> a)
                    ().asRight
                  case Variable(_, t) if t == a => ().asRight
                  case _                        => ArgsTypeMisMatch().asLeft
                }
            }
          } yield f.retType
      }
    }

    applyCheck(defun.body).map { ret =>
      // TODO error will infer type
      val argsType = defun.args.map(fa => variableTypeMap(fa.sym))
      JispType.FunctionType(argsType, ret)
    }
  }
  def inferMain(body: List[JList]): JispType.FunctionType = {
    ???
  }

  sealed trait TypeMisMatch
  case class ToManyArgs() extends TypeMisMatch
  case class ArgsTypeMisMatch() extends TypeMisMatch
  case class FunctionNotFound() extends TypeMisMatch
  case class VaeiableNotFound() extends TypeMisMatch

  sealed trait TypeInfo
  case class Variable(sym: Symbol, tpe: JispType)
  case class Constant(tpe: JispType)
}
