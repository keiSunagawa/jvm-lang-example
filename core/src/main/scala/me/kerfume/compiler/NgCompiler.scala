package me.kerfume.compiler

import me.kerfume.jisp.NgAST._
import me.kerfume.assembly._
import DSL._
import me.kerfume.jisp.JispType
import cats.data.Reader
import org.atnos.eff.|=

import NgCompiler._

import me.kerfume.jisp.type_modules.TypeModule
import cats.data.State

object NgCompiler {
  type WithError[A] = Either[Error, A]
  type _withError[R] = WithError |= R
  type WithState[A] = State[Context, A]
  type _withState[R] = WithState |= R

  case class Context(
      varMap: Map[String, VarRef],
      fMap: Map[String, JispType.FunctionType]
  )

  def buildClass(mainLogic: List[Command], methods: List[Method]): Clazz = {
    val main = defm("main")("[Ljava/lang/String;")(
      Void
    )(
      mainLogic: _*
    )

    val fullDefs = me.kerfume.assembly.BuildIn.all ++ methods :+ main
    defc("", "JispCode")(fullDefs: _*)
  }

  case class VarRef(offset: Int, tpe: JispType)

  sealed trait Error
}

// FIXME: unsafe  codes
trait NgCompiler {
  protected def numberingVar(
      varList: List[(String, JispType)]
  ): Map[String, VarRef] = {
    varList
      .traverse {
        case (sym, t) =>
          State { ofs: Int =>
            val addOfs = if (t == JispType.Number) 2 else 1
            (ofs + addOfs) -> (sym -> VarRef(ofs, t))
          }
      }
      .runA(0)
      .value
      .toMap
  }
  protected def applyCompile(
      ap: Apply,
      ctx: Context
  ): List[Command] = {
    val tCommands = ap.params.flatMap {
      case n: Num => const(n.value) :: Nil
      case s: Str => const(s.value) :: Nil
      case s: Symbol =>
        val v = ctx.varMap(s.value)
        load(v.offset, typeConv(v.tpe)) :: Nil
      case xs: Apply => applyCompile(xs, ctx)
      case _         => throw new RuntimeException("unreachable code ><")
    }
    val fName = ap.f
    val hCommand = call(fNameConv(fName.value, ctx.fMap(fName.value)))
    tCommands :+ hCommand
  }

  // assembly側の責務
  protected def typeConvStr(jt: JispType): String = jt match {
    case JispType.Number => "J"
    case JispType.String => "Ljava/lang/String;"
    case JispType.Void   => "V"
  }
  protected def typeConv(jt: JispType): Tpe = jt match {
    case JispType.Number => L
    case JispType.String => ref(Tpe.String)
    case JispType.Void   => Void
  }
  protected def fNameConv(
      name: String,
      ftype: JispType.FunctionType
  ): String = {
    val argTypes = ftype.argsType.map(typeConvStr).mkString
    val retType = typeConvStr(ftype.retType)
    s"JispCode/${name}(${argTypes})$retType"
  }
}
