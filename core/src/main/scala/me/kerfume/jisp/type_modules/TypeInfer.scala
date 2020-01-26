package me.kerfume.jisp.type_modules

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import me.kerfume.jisp.JispType
import me.kerfume.jisp.NgAST._
import TypeModule._
import cats.data.State
import TypeInfer._

object TypeInfer {
  type WithState[A] = State[Context, A]
  type _withState[R] = WithState |= R
  type WithError[A] = Either[Error, A]
  type _withError[R] = WithError |= R

  sealed trait Error
  case class ToManyArgs(invalid: Apply) extends Error
  case class ArgsTypeMisMatch(invalid: Apply) extends Error
  case class FunctionNotFound(invalid: Apply) extends Error
  case class VariableNotFound(invalid: Apply, symbol: Symbol) extends Error
  case class MissInferArgsType(invalid: Defun) extends Error
  case class VoidBind(invalid: Let) extends Error

  sealed trait TypeInfo
  case class Variable(sym: Symbol, tpe: InferState[JispType.FunArgType])
      extends TypeInfo
  case class Constant(tpe: JispType) extends TypeInfo

}
trait TypeInfer {
  protected def initialVar[R: _withState](
      name: Symbol
  ): Eff[R, Unit] = {
    modify[R, Context](_.initialVar(name))
  }
  protected def putVar[R: _withState](
      name: Symbol,
      tpe: JispType.FunArgType
  ): Eff[R, Unit] = {
    modify[R, Context](_.putVar(name, tpe))
  }
  protected def putF[R: _withState](
      name: Symbol,
      tpe: JispType.FunctionType
  ): Eff[R, Unit] = {
    modify[R, Context](_.putF(name, tpe))
  }

  protected def applyInfer[R: _withState: _withError](
      exp: Apply
  ): Eff[R, JispType] = {
    type F[A] = Eff[R, A]

    def paramCheck: Value => F[TypeInfo] = {
      case xs: Apply =>
        applyInfer(xs).map(Constant(_))
      case _: Num => right(Constant(JispType.Number))
      case _: Str => right(Constant(JispType.String))
      case x: Symbol =>
        for {
          ctx <- get
          inf <- ctx.varMap
            .get(x.value)
            .map(Variable(x, _))
            .toRight[Error](VariableNotFound(exp, x))
            .send
        } yield inf
    }
    def getFunction(
        name: Symbol,
        paramSize: Int
    ): F[JispType.FunctionType] = {
      for {
        ctx <- get
        f <- ctx.fMap
          .get(name.value)
          .toRight[Error](FunctionNotFound(exp))
          .send
        _ <- Either
          .cond(
            f.argsType.size == paramSize,
            (),
            ToManyArgs(exp): Error
          )
          .send
      } yield f
    }

    def typeCheck(p: TypeInfo, a: JispType.FunArgType): F[Unit] = {
      p match {
        case Constant(t) if t == a            => unit
        case Variable(sym, WillInfer)         => putVar(sym, a)
        case Variable(_, Inferd(t)) if t == a => unit
        case _                                => left(ArgsTypeMisMatch(exp): Error)
      }
    }

    for {
      params <- exp.params.traverse { paramCheck }
      f <- getFunction(exp.f, params.size)
      _ <- params.zip(f.argsType).traverse { (typeCheck _).tupled }
    } yield f.retType
  }
}
