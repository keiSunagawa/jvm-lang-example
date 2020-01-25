package me.kerfume.compiler

import me.kerfume.jisp.NgAST._
import me.kerfume.assembly._
import org.atnos.eff._
import org.atnos.eff.state._
import org.atnos.eff.syntax.state._
import NgCompiler._
import me.kerfume.jisp.type_modules.TypeModule
import me.kerfume.assembly.DSL._
import me.kerfume.Eff._
import me.kerfume.jisp.type_modules.TypeModule.Inferd

object StatementCompiler extends NgCompiler {
  def compile[R: _withError](
      statements: List[Statement],
      typeContext: TypeModule.Context
  ): Eff[R, List[Command]] = {
    val context = Context(
      numberingVar(typeContext.varMap.toList.map {
        // FIXME pattern match safetiy
        case (name, Inferd(tpe)) => name -> tpe
      }),
      typeContext.fMap
    )
    compile0[WithState +: R](statements).evalState(context)
  }

  private def compile0[R: _withError: _withState](
      statements: List[Statement]
  ): Eff[R, List[Command]] = {
    statements.flatTraverse {
      case ap: Apply =>
        get.map { ctx =>
          applyCompile(ap, ctx)
        }
      case lt: Let => letCompile(lt)
    }
  }
  private def letCompile[R: _withError: _withState](
      let: Let
  ): Eff[R, List[Command]] = {
    for {
      ctx <- get
      commands = applyCompile(let.body, ctx)
      varRef = ctx.varMap(let.name.value)
    } yield {
      commands :+ store(varRef.offset, typeConv(varRef.tpe))
    }
  }
}
