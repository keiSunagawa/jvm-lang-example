package me.kerfume.assembly

object DSL {
  // command
  def const(i: Long): Command = NConst(i)
  def const(s: String): Command = SConst(s)
  def addN: Command = RawCommand("ladd", Nil)
  def store(i: Int, tpe: Tpe): Command = Store(i, tpe)
  def load(i: Int, tpe: Tpe): Command = Load(i, tpe)
  def printN(valIndex: Int): Command = Print(valIndex, L)
  def printS(valIndex: Int): Command = Print(valIndex, ref(Tpe.String))
  def printO(valIndex: Int): Command = Print(valIndex, ref("undefined"))
  def call(mname: String): Command = Call(mname)

  // method
  def defm(name: String)(args: String*)(ret: Tpe)(body: Command*): Method =
    Method(name, args.toList, body.toList, ret)

  // class
  def defc(pkg: String, name: String)(methods: Method*): Clazz =
    Clazz(pkg, name, methods.toList)
}

object Test {
  import DSL._
  def run(): Unit = {
    val clazz = defc("com.example", "Example")(
      BuildIn.plus,
      defm("main")("[Ljava/lang/String;")(Void)(
        const("aa"),
        store(1, ref(Tpe.String)),
        printS(1),
        const(3),
        const(4),
        call("plus(JJ)J"),
        store(1, L),
        printN(1)
      )
    )
    val dump = Dumper.dump(clazz)
    println(dump)
  }
}
