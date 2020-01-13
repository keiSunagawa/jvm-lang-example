package me.kerfume.assembly

object DSL {
  // command
  def const(i: Int): Command = RawCommand("iconst", List(i.toString))
  def iadd: Command = RawCommand("iadd", Nil)
  def store(i: Int): Command = Store(i)
  def print(valIndex: Int): Command = Print(valIndex)

  // method
  def defm(name: String)(args: String*)(ret: String)(body: Command*): Method =
    Method(name, args.toList, body.toList, ret)

  // class
  def defc(pkg: String, name: String)(methods: Method*): Clazz =
    Clazz(pkg, name, methods.toList)
}

object Test {
  import DSL._
  def run(): Unit = {
    val clazz = defc("com.example", "Example")(
      defm("main")("[Ljava/lang/String;")("V")(
        const(3),
        const(4),
        iadd,
        store(1),
        print(1)
      )
    )
    val dump = Dumper.dump(clazz)
    println(dump)
  }
}
