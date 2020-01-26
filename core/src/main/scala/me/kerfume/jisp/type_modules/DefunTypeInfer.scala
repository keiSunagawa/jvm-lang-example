package me.kerfume.jisp.type_modules

import cats.syntax.foldable._
import org.atnos.eff._
import org.atnos.eff.state._
import org.atnos.eff.either._
import org.atnos.eff.syntax.state._
import me.kerfume.jisp.JispType
import me.kerfume.jisp.NgAST._
import TypeInfer._
import me.kerfume.jisp.type_modules.TypeModule.WillInfer
import me.kerfume.jisp.type_modules.TypeModule.Inferd
import TypeModule._
import me.kerfume.Eff._

object DefunTypeInfer extends TypeInfer {
  def infer[R: _withError](
      defun: Defun,
      in: Context
  ): Eff[R, (Symbol, JispType.FunctionType)] = {
    infer0[WithState +: R](defun).evalState(in)
  }

  private def infer0[R: _withState: _withError](
      defun: Defun
  ): Eff[R, (Symbol, JispType.FunctionType)] = {
    for {
      _ <- defun.args.traverse_(initialVar(_))
      retType <- applyInfer(defun.body)
      ctx <- get
      argsTypes = defun.args.map(a => ctx.varMap(a.value))
      argsTypesStrict <- argsTypes
        .traverse {
          case WillInfer =>
            left[R, Error, JispType.FunArgType](
              MissInferArgsType(defun): Error
            )
          case Inferd(t) => right(t)
        }
      ft = JispType.FunctionType(argsTypesStrict, retType)
    } yield (defun.name -> ft)
  }
}
