package me.kerfume.jisp

import cats.syntax.either._
import cats.instances.either._
import me.kerfume.jisp.JispType.WillInfer
import cats.data.StateT

object TypeInfer {
  def applyCheckS(
      exp: JList,
      functionTypeMap: Map[String, JispType.FunctionType]
  ): WithVMapT[JispType] = {
    def paramCheck: Value => WithVMapT[TypeInfo] = {
      case xs: JList =>
        applyCheckS(xs, functionTypeMap).map(Constant(_))
      case _: Num => StateT.liftF { Constant(JispType.Number).asRight }
      case _: Str => StateT.liftF { Constant(JispType.String).asRight }
      case x: Symbol =>
        StateT.inspectF { m =>
          m.get(x)
            .map(Variable(x, _))
            .toRight(VaeiableNotFound())
        }
    }

    def getFunction(
        name: String,
        paramSize: Int
    ): Either[TypeMisMatch, JispType.FunctionType] = {
      for {
        f <- functionTypeMap
          .get(name)
          .toRight(FunctionNotFound())
        _ <- Either.cond(
          f.argsType.size == paramSize,
          (),
          ToManyArgs()
        )
      } yield f

    }
    def typeCheck(p: TypeInfo, a: JispType): WithVMapT[Unit] = {
      p match {
        case Constant(t) if t == a    => StateT.pure { () }
        case Variable(sym, WillInfer) => put(sym, a)
        case Variable(_, t) if t == a => StateT.pure { () }
        case _                        => StateT.liftF { ArgsTypeMisMatch().asLeft }
      }
    }

    exp.values match {
      case Nil => StateT.pure { JispType.Void }
      case Symbol(sym) :: other =>
        for {
          params <- other.traverse { paramCheck }
          f <- StateT.liftF { getFunction(sym, params.size) }
          _ <- params.zip(f.argsType).traverse { (typeCheck _).tupled }
        } yield f.retType
    }
  }

  def infer(
      defun: Defun,
      functionTypeMap: Map[String, JispType.FunctionType]
  ): Either[TypeMisMatch, JispType.FunctionType] = {
    (for {
      // initialize
      _ <- defun.args.traverse(a => put(a.sym, a.tpe))
      retType <- applyCheckS(defun.body, functionTypeMap)
      ft <- StateT.inspect { m =>
        val argsTypes = defun.args.map(fa => m(fa.sym))
        JispType.FunctionType(argsTypes, retType)
      }: WithVMapT[JispType.FunctionType]
    } yield ft).runA(Map.empty)
  }

  def inferMain(
      body: List[Statement],
      functionTypeMap: Map[String, JispType.FunctionType]
  ): Either[TypeMisMatch, Map[Symbol, JispType]] = {
    // TODO applyCheckに対してmapするだけ？letへの対応も必要
    body
      .traverse {
        case Apply(b) => applyCheckS(b, functionTypeMap).void
        case Let(name, b) =>
          for {
            t <- applyCheckS(b, functionTypeMap)
            _ <- put(name, t)
          } yield ()
      }
      .runS(Map.empty)
  }

  type WithVMapT[A] = StateT[
    ({ type T[X] = Either[TypeMisMatch, X] })#T,
    Map[Symbol, JispType],
    A
  ]

  private def put(name: Symbol, tpe: JispType): WithVMapT[Unit] =
    StateT.modify(m => m + (name -> tpe))

  sealed trait TypeMisMatch
  case class ToManyArgs() extends TypeMisMatch
  case class ArgsTypeMisMatch() extends TypeMisMatch
  case class FunctionNotFound() extends TypeMisMatch
  case class VaeiableNotFound() extends TypeMisMatch

  sealed trait TypeInfo
  case class Variable(sym: Symbol, tpe: JispType) extends TypeInfo
  case class Constant(tpe: JispType) extends TypeInfo
}
