package me.kerfume.jisp.antlr

import me.kerfume.jisp.antlr.JispBaseListener
import me.kerfume.jisp.JList
import me.kerfume.jisp.antlr.JispParser.ListContext
import scala.jdk.CollectionConverters._
import me.kerfume.jisp.antlr.JispParser.ValueContext
import me.kerfume.jisp.Value
import me.kerfume.jisp._
import me.kerfume.jisp.antlr.JispParser.ErrorContext
import me.kerfume.jisp.antlr.JispParser.StmtsContext

class ASTConv extends JispBaseListener {
  var stmts: List[JList] = Nil

  override def enterError(ctx: ErrorContext): Unit = {
    ctx.exception.printStackTrace()
  }

  override def enterStmts(ctx: StmtsContext): Unit = {
    println("enter stmts")
  }
  override def enterList(ctx: ListContext): Unit = {
    println("enter list..")
    stmts = JList(ctx.value().asScala.toList.map(getValue)) :: stmts
  }

  private def getValue(ctx: ValueContext): Value = {
    if (ctx.NUMBER() != null) Num(ctx.NUMBER().getText().toLong)
    else if (ctx.STRING != null) Str(ctx.STRING().getText())
    else Symbol(ctx.SYMBOl.getText())
  }
}
