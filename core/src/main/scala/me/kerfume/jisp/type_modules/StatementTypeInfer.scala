package me.kerfume.jisp.type_modules

import cats.syntax.foldable._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import me.kerfume.jisp.JispType
import me.kerfume.jisp.NgAST._
import TypeInfer._

object StatementTypeInfer extends TypeInfer {
  def infer[R: _withError: _withState](
      body: List[Statement]
  ): Eff[R, Unit] = {
    body
      .traverse_ {
        case ap: Apply => applyInfer(ap).void
        case lt: Let =>
          for {
            t <- applyInfer(lt.body)
            tNonVoid <- JispType.toNonVoid(t).toRight(VoidBind(lt): Error).send
            _ <- putVar(lt.name, tNonVoid)
          } yield ()
      }
  }
}
