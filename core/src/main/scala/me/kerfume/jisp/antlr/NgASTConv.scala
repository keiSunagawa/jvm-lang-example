package me.kerfume.jisp.antlr

import me.kerfume.jisp.antlr.JispBaseListener
import me.kerfume.jisp.antlr.JispParser.ListContext
import me.kerfume.jisp.antlr.JispParser.ValueContext
import me.kerfume.jisp.NgAST._
import me.kerfume.jisp.CodeInfo
import me.kerfume.jisp.antlr.JispParser.ErrorContext
import me.kerfume.jisp.antlr.JispParser.StmtsContext
import org.antlr.v4.runtime.ParserRuleContext

class NgASTConv extends JispBaseListener {
  var stmts: List[ValueList] = Nil
  var currentJList: Map[Int, ValueList] = Map.empty
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
    val info = getInfo(ctx)
    currentJList = currentJList + (deps -> ValueList(Nil, info))
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
    val info = getInfo(ctx)
    if (ctx.NUMBER() != null) Num(ctx.NUMBER().getText().toLong, info)
    else if (ctx.STRING != null) Str(ctx.STRING().getText(), info)
    else Symbol(ctx.SYMBOl.getText(), info)
  }

  private def getInfo(ctx: ParserRuleContext): CodeInfo = {
    val line = ctx.getStart().getLine()
    val start = ctx.getStart().getStartIndex()
    val end = ctx.getStop().getStopIndex()
    val text = ctx.getText() // ignore white space
    CodeInfo(line, start, end, text)
  }
}
