package me.kerfume.jisp.type_modules

import me.kerfume.jisp.JispType
import me.kerfume.jisp.NgAST._

object TypeModule {
  sealed trait InferState[+A]
  case object WillInfer extends InferState[Nothing]
  case class Inferd[A](tpe: A) extends InferState[A]

  case class Context(
      fMap: Map[String, JispType.FunctionType],
      varMap: Map[String, InferState[JispType.FunArgType]]
  ) {
    def initialVar(
        name: Symbol
    ): Context = {
      val newVarMap = varMap + (name.value -> WillInfer)
      copy(varMap = newVarMap)
    }

    def putVar(
        name: Symbol,
        tpe: JispType.FunArgType
    ): Context = {
      val newVarMap = varMap + (name.value -> Inferd(tpe))
      copy(varMap = newVarMap)
    }
    def putF(
        name: Symbol,
        tpe: JispType.FunctionType
    ): Context = {
      val newFMap = fMap + (name.value -> tpe)
      copy(fMap = newFMap)
    }
  }
  object Context {
    val empty = Context(Map.empty, Map.empty)
  }
}
