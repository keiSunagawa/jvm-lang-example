package me.kerfume

import org.antlr.v4.runtime.CommonTokenStream
import me.kerfume.jisp.antlr.ASTConv
import me.kerfume.jisp.antlr.JispLexer
import me.kerfume.jisp.antlr.JispParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.tree.ParseTreeWalker
import me.kerfume.jisp.RuleChecker
import me.kerfume.jisp.DefunCollector
import me.kerfume.jisp.TypeInfer
import me.kerfume.jisp.JispType
import me.kerfume.jisp.LetCollector
import cats.instances.either._

object Main extends App {

  def parse(input: String) = {
    println("parse start to " + input)

    val charStream = CharStreams.fromString(input)
    val lexer = new JispLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new JispParser(tokens)

    val converter = new ASTConv()
    val walker = new ParseTreeWalker();
    /* Implement listener and use parser */
    walker.walk(converter, parser.stmts);
    converter.stmts.reverse
  }

  val res = parse("""(defun f ((t:num x) (t:num y))
                    |   (plus x (plus 1 y)))
                    |(let z (f 1 3))
                    |(f z 1)
                    """.stripMargin)
  println(res)
  println(RuleChecker.checkListHead(res))
  println(RuleChecker.checkMetaSymbol(res))
  println(RuleChecker.checkDefunSymbol(res))
  println(RuleChecker.checkLetSymbol(res))
  println(DefunCollector.collect(res))
  val (ss, defs) = DefunCollector.collect(res).right.get

  val plus = JispType.FunctionType(
    JispType.Number :: JispType.Number :: Nil,
    JispType.Number
  )
  val stmts = LetCollector.collect(ss).toOption.get
  println(stmts)

  val fMap = defs
    .traverse { d =>
      TypeInfer.infer(d, Map("plus" -> plus)).map { d.name.value -> _ }
    }
    .toOption
    .get

  println(fMap)

  println(TypeInfer.inferMain(stmts, fMap.toMap))
}
