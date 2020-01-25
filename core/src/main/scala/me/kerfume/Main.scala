package me.kerfume

import org.antlr.v4.runtime.CommonTokenStream
import me.kerfume.jisp.antlr.NgASTConv
import me.kerfume.jisp.antlr.JispLexer
import me.kerfume.jisp.antlr.JispParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.tree.ParseTreeWalker
import me.kerfume.jisp.JispType
import me.kerfume.assembly.Dumper
import me.kerfume.compiler._
import me.kerfume.jisp.ruler._
import cats.syntax.semigroupk._
import cats.syntax.foldable._
import me.kerfume.jisp.ruler._
import me.kerfume.jisp.collector._
import me.kerfume.jisp.NgAST._
import me.kerfume.jisp.type_modules._
import MainHelper._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Main extends App {
  ng()

  def ng(): Unit = {
    def rule(in: List[JList]): Ruler.Result[List[JList]] = {
      for {
        check1 <- ApplyRuler.rule(in).toEither
        _ <- (DefunRuler.rule(check1) <+> LetRuler.rule(check1)).toEither
      } yield check1
    }

    def parse(input: String) = {
      println("parse start to " + input)

      val charStream = CharStreams.fromString(input)
      val lexer = new JispLexer(charStream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new JispParser(tokens)

      val converter = new NgASTConv()
      val walker = new ParseTreeWalker();
      /* Implement listener and use parser */
      walker.walk(converter, parser.stmts);
      converter.stmts.reverse
    }

    val res = parse("""(defun f (x y)
                    |   (plus x (plus 1 y)))
                    |(let z (f 1 2))
                    |(f z 1)
                    """.stripMargin)

    val plus = JispType.FunctionType(
      JispType.Number :: JispType.Number :: Nil,
      JispType.Number
    )

    def compile[
        R: Ruler._result: DefunCollector._result: LetCollector._result: TypeInfer._withState: TypeInfer._withError: NgCompiler._withError
    ] =
      for {
        codes <- rule(res).send
        defunWithStmts <- DefunCollector.collect(codes).send
        (rowStmts, defuns) = defunWithStmts
        stmts <- LetCollector.collect(rowStmts).send
        initial <- get
        _ <- defuns.traverse_ {
          DefunTypeInfer.infer(_, initial) >>= {
            case (sym, ft) =>
              modify[R, TypeModule.Context] { _.putF(sym, ft) }
          }
        }
        _ <- StatementTypeInfer.infer(stmts)
        ctx <- get
        methods <- defuns.traverse { d =>
          MethodCompiler.compile(d, ctx).send
        }
        mainLogic <- StatementCompiler.compile(stmts, ctx)
        clazz = NgCompiler.buildClass(mainLogic, methods)
      } yield Dumper.dump(clazz)

    val x = compile[
      Fx.fx7[
        Ruler.Result,
        DefunCollector.Result,
        LetCollector.Result,
        TypeInfer.WithState,
        TypeInfer.WithError,
        NgCompiler.WithError,
        Option
      ]
    ].evalState(TypeModule.Context.empty.putF(Symbol("plus", null), plus))
      .runEither[Ruler.Error]
      .handle
      .runEither[DefunCollector.Error]
      .handle
      .runEither[LetCollector.Error]
      .handle
      .runEither[TypeInfer.Error]
      .handle
      .runEither[NgCompiler.Error]
      .handle
      .runOption
      .run
      .get

    println(x)
  }

}

object MainHelper {
  implicit class Handler[E, A, R: _option](
      val withE: Eff[R, Either[E, A]]
  ) {
    def handle: Eff[R, A] = {
      withE.flatMap { errorHandling(_) }
    }

    private def errorHandling[R2: _option](
        eor: Either[E, A]
    ): Eff[R2, A] =
      eor match {
        case Left(e) =>
          println(e)
          none
        case Right(a) => some(a)
      }
  }
}
