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
import me.kerfume.assembly.BuildIn
import me.kerfume.assembly.Dumper

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
                    |(let z (f 1 2))
                    |(f z 1)
                    """.stripMargin)

  import org.atnos.eff._
  import org.atnos.eff.all._
  import org.atnos.eff.syntax.all._
  import cats.syntax.semigroupk._
  import cats.instances.list._
  import cats.instances.either._

  val plus = JispType.FunctionType(
    JispType.Number :: JispType.Number :: Nil,
    JispType.Number
  )
  val buildInF = Map("plus" -> plus)

  def x[
      R: RuleChecker._result: DefunCollector._result: LetCollector._result: TypeInfer._result
  ] =
    for {
      _ <- (RuleChecker.checkListHead(res) <+>
        RuleChecker.checkMetaSymbol(res) <+>
        RuleChecker.checkDefunSymbol(res) <+>
        RuleChecker.checkLetSymbol(res)).toEither.send

      collectRes <- (DefunCollector.collect(res)).send
      (letOrApplies, defs) = collectRes
      stmts <- LetCollector.collect(letOrApplies).send

      fMap <- defs.traverse { d =>
        TypeInfer.infer(d, buildInF).map { d.name.value -> _ }
      }.send
      fMapFull = buildInF ++ fMap.toMap
      varMap <- TypeInfer.inferMain(stmts, fMapFull).send
    } yield {
      val asmFs = me.kerfume.compiler.Compiler.compile(defs, fMapFull)
      val mainCs =
        me.kerfume.compiler.Compiler.compileMain(stmts, varMap, fMapFull)
      import me.kerfume.assembly._
      val ms = asmFs :+ BuildIn.plus :+ DSL.defm("main")("[Ljava/lang/String;")(
        Void
      )(
        mainCs: _*
      )
      val czz = DSL.defc("", "JispCode")(ms: _*)
      Dumper.dump(czz)
    }

  def toOption[E, A, R: _option](eor: Either[E, A]): Eff[R, A] = eor match {
    case Left(e) =>
      println(e)
      none
    case Right(a) => some(a)
  }

  type Stack = Fx.fx5[
    RuleChecker.Result,
    DefunCollector.Result,
    LetCollector.Result,
    TypeInfer.Result,
    Option
  ]
  val y = x[Stack]
    .runEither[RuleChecker.Error]
    .flatMap {
      toOption(_)
    }
    .runEither[DefunCollector.Error]
    .flatMap {
      toOption(_)
    }
    .runEither[LetCollector.Error]
    .flatMap {
      toOption(_)
    }
    .runEither[TypeInfer.Error]
    .flatMap {
      toOption(_)
    }
    .runOption
    .run

  println(y.get)
}
