package me.kerfume.assembly

import cats.data.Writer
import cats.Monoid
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.flatMap._

object Dumper {
  type WL[A, B] = Writer[List[A], B]
  object WL {
    def apply[A, B](a: A, b: B): WL[A, B] =
      Writer.apply[List[A], B](List(a), b)

    def value[A: Monoid, B](b: B): WL[A, B] = Writer.value[List[A], B](b)

    def tell[A](a: A): Writer[List[A], Unit] = Writer.tell(List(a))

    def listen[A, B](writer: WL[A, B]): Writer[List[A], (B, List[A])] =
      Writer.listen(writer)
  }
  import WL._

  private val idt = " " * 4
  def dump(clazz: Clazz): String = {
    val res = for {
      _ <- tell(s".class public ${clazz.pkg}.${clazz.name}")
      _ <- tell(".super java/lang/Object")
      _ <- tell(s""".method public <init>()V
                    |${idt}aload_0
                    |${idt}invokenonvirtual java/lang/Object/<init>()V
                    |${idt}return
                    |.end method""".stripMargin)
      // dump method
      _ <- clazz.methods.traverse(dumpMethod)
    } yield ()

    res.run._1.mkString("\n")
  }

  def dumpMethod(m: Method): WL[String, Unit] = {
    for {
      _ <- tell(
        s".method public static ${m.name}(${m.args.mkString})${typeToString(m.ret)}"
      )
      _ <- tell(s"${idt}.limit stack 10")
      _ <- tell(s"${idt}.limit locals 10")
      _ <- m.body
        .traverse(dumpCommand)
        .mapWritten(xs => xs.map(x => s"${idt}${x}"))
      _ <- tell(s"${idt}${dumpRetrun(m.ret)}")
      _ <- tell(".end method")
    } yield ()
  }

  def dumpCommand(c: Command): WL[String, Unit] = {
    c match {
      case Store(i, tpe)     => tell(s"${prefix(tpe)}store $i")
      case Load(i, tpe)      => tell(s"${prefix(tpe)}load $i")
      case NConst(l)         => tell(s"ldc2_w $l")
      case SConst(s)         => tell(("ldc_w " + "\"" + s + "\""))
      case RawCommand(n, os) => tell(s"${n} ${os.mkString(" ")}")
      case Call(m) =>
        for {
          _ <- tell(s"invokestatic com/example/Example/${m}")
        } yield ()
      case Print(vi, tpe) =>
        for {
          _ <- tell(s"getstatic java/lang/System/out Ljava/io/PrintStream;")
          _ <- tpe match {
            case L =>
              tell(s"lload ${vi}") >> tell(
                s"invokevirtual java/io/PrintStream/println(J)V"
              )
            case ref(v) if v == Tpe.String =>
              tell(s"aload ${vi}") >> tell(
                s"invokevirtual java/io/PrintStream/println(${Tpe.String})V"
              )
            case _ =>
              tell(s"aload ${vi}") >> tell(
                s"invokevirtual java/io/PrintStream/println(${Tpe.Object})V"
              )
          }
        } yield ()
    }
  }

  def prefix(tpe: Tpe, deps: Int = 0): String = tpe match {
    case L         => "l"
    case array(et) => if (deps >= 1) "a" else ("a" + prefix(et, deps + 1))
    case ref(_)    => "a"
    case Void      => "a"
  }
  def dumpRetrun(tpe: Tpe, deps: Int = 0): String = tpe match {
    case L         => "lreturn"
    case array(et) => if (deps >= 1) "areturn" else ("a" + prefix(et, deps + 1))
    case ref(_)    => "areturn"
    case Void      => "return"
  }
  def typeToString(tpe: Tpe): String = tpe match {
    case L         => "J"
    case array(et) => "[" + typeToString(et)
    case ref(s)    => s
    case Void      => "V"
  }
}
