package me.kerfume.jisp.antlr

import me.kerfume.jisp.antlr.JispBaseListener
import me.kerfume.jisp.JList
import me.kerfume.jisp.antlr.JispParser.ListContext
import me.kerfume.jisp.antlr.JispParser.ValueContext
import me.kerfume.jisp.Value
import me.kerfume.jisp._
import me.kerfume.jisp.antlr.JispParser.ErrorContext
import me.kerfume.jisp.antlr.JispParser.StmtsContext

class ASTConv extends JispBaseListener {
  var stmts: List[JList] = Nil
  var currentJList: Map[Int, JList] = Map.empty
  var deps: Int = 0

  override def enterError(ctx: ErrorContext): Unit = {
    ctx.exception.printStackTrace()
  }

  override def enterStmts(ctx: StmtsContext): Unit = {
    println("enter stmts")
  }

  override def enterList(ctx: ListContext): Unit = {
    println("enter list..")
    deps += 1
    currentJList = currentJList + (deps -> JList(Nil))
  }
  override def exitList(ctx: ListContext): Unit = {
    println("exit list..")

    val cd = deps
    deps -= 1
    if (deps == 0) {
      stmts = currentJList(cd) :: stmts
    } else {
      val rml = currentJList(cd)
      currentJList - cd
      val cl = currentJList(deps)
      currentJList = currentJList + (deps -> cl.add(rml))
    }
  }

  override def enterValue(ctx: ValueContext): Unit = {
    val cl = currentJList(deps)
    currentJList = currentJList + (deps -> cl.add(getValue(ctx)))
  }

  private def getValue(ctx: ValueContext): Value = {
    if (ctx.NUMBER() != null) Num(ctx.NUMBER().getText().toLong)
    else if (ctx.STRING != null) Str(ctx.STRING().getText())
    else Symbol(ctx.SYMBOl.getText())
  }
}
