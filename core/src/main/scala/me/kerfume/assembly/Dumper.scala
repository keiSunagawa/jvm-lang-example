package me.kerfume.assembly
import java.io.StringWriter

object Dumper {
  def dump(clazz: Clazz): String = {
    val writer = new StringWriter()
    writer.writel(s".class public ${clazz.pkg}.${clazz.name}")
    writer.writel(".super java/lang/Object")
    writer.writel(s""".method public <init>()V
                    |    aload_0
                    |    invokenonvirtual java/lang/Object/<init>()V
                    |    return
                    |.end method""".stripMargin)
    // dump method

    writer.toString()
  }
  implicit class WW(val w: StringWriter) {
    def writel(line: String): Unit = w.write(line + "\n")
  }
}
