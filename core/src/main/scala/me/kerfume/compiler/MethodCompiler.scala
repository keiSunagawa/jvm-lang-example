package me.kerfume.compiler

import me.kerfume.jisp.type_modules.TypeModule
import me.kerfume.jisp.NgAST._
import me.kerfume.assembly._
import NgCompiler._
import org.atnos.eff.Fx

object MethodCompiler extends NgCompiler {
  def compile(
      defun: Defun,
      typeContext: TypeModule.Context
  ): WithError[Method] = {
    val fType = typeContext.fMap(defun.name.value)
    val context = {
      val varList =
        defun.args.zip(fType.argsType).map { case (s, tp) => s.value -> tp }
      val offsetMap = numberingVar(varList)
      Context(
        offsetMap,
        typeContext.fMap
      )
    }

    val argTypes = fType.argsType.map { typeConvStr }
    val retType = typeConv(fType.retType)

    val body = applyCompile(defun.body, context)

    Right(
      Method(defun.name.value, argTypes, body, retType)
    )
  }
}
