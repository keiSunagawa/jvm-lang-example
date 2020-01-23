package me.kerfume.compiler

import me.kerfume.assembly._
import me.kerfume.jisp.Defun
import me.kerfume.jisp._
import cats.data.`package`.State

object Compiler {
  def compile(
      defuns: List[Defun],
      typeMap: Map[String, JispType.FunctionType]
  ): List[Method] = {
    defuns.map { df =>
      val tpe = typeMap(df.name.value)
      compile0(df, tpe, typeMap)
    }
  }
  private def compile0(
      defun: Defun,
      tpe: JispType.FunctionType,
      typeMap: Map[String, JispType.FunctionType]
  ): Method = {
    val argSymbolMap =
      defun.args.zip(tpe.argsType).map { case (a, t) => a.sym -> t }
    val args = tpe.argsType.map { typeConvStr }
    val body = bodyCompile(argSymbolMap, defun.body, typeMap)
    val ret = typeConv(tpe.retType)

    Method(defun.name.value, args, body, ret)
  }

  private def bodyCompile(
      argsMap: List[(Symbol, JispType)],
      body: JList,
      typeMap: Map[String, JispType.FunctionType]
  ): List[Command] = {
    import me.kerfume.assembly.DSL._
    val argsWithOffset = argsMap
      .traverse {
        case (sym, t) =>
          State { ofs: Int =>
            val addOfs = if (t == JispType.Number) 2 else 1
            (ofs + addOfs) -> (sym -> (ofs -> t))
          }
      }
      .runA(0)
      .value
      .toMap

    val h :: t = body.values
    val tCommands = t.flatMap {
      case Num(n) => const(n) :: Nil
      case Str(s) => const(s) :: Nil
      case s: Symbol =>
        val (ofs, t) = argsWithOffset(s)
        load(ofs, typeConv(t)) :: Nil
      case xs: JList => bodyCompile(argsMap, xs, typeMap)
    }
    val fName = h.asInstanceOf[Symbol].value
    val hCommand = call(fNameConv(fName, typeMap(fName)))
    tCommands :+ hCommand
  }

  def compileMain(
      stmts: List[Statement],
      tpeMap: Map[Symbol, JispType],
      typeMap: Map[String, JispType.FunctionType]
  ): List[Command] = {
    import me.kerfume.assembly.DSL._

    val varWithOffset = tpeMap.toList
      .traverse {
        case (sym, t) =>
          State { ofs: Int =>
            val addOfs = if (t == JispType.Number) 2 else 1
            (ofs + addOfs) -> (sym -> (ofs -> t))
          }
      }
      .map(_.toMap)
      .runA(0)
      .value
//    var offset_ = offset

    def applyCompile(ap: JList): List[Command] = {
      val h :: t = ap.values
      val tCommands = t.flatMap {
        case Num(n) => const(n) :: Nil
        case Str(s) => const(s) :: Nil
        case s: Symbol =>
          val (ofs, t) = varWithOffset(s)
          load(ofs, typeConv(t)) :: Nil
        case xs: JList => applyCompile(xs)
      }
      val fName = h.asInstanceOf[Symbol].value
      val hCommand = call(fNameConv(fName, typeMap(fName)))
      tCommands :+ hCommand
    }

    def letCompile(let: Let): List[Command] = {
      val bodyCs = applyCompile(let.body)
      val bindCs = {
        val (ofs, t) = varWithOffset(let.name)
        store(ofs, typeConv(t))
      }
      bodyCs :+ bindCs
    }
    stmts.flatMap {
      case let: Let  => letCompile(let)
      case ap: Apply => applyCompile(ap.body)
    }
  }

  private def typeConvStr(jt: JispType): String = jt match {
    case JispType.Number => "J"
    case JispType.String => "Ljava/lang/String;"
    case _               => ???
  }
  private def typeConv(jt: JispType): Tpe = jt match {
    case JispType.Number => L
    case JispType.String => ref(Tpe.String)
    case _               => ???
  }
  private def fNameConv(name: String, ftype: JispType.FunctionType): String = {
    val argTypes = ftype.argsType.map(typeConv).mkString
    val retType = typeConv(ftype.retType)
    s"JispCode/${name}(${argTypes})$retType"
  }
}
