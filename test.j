.class public com.example.Example
.super java/lang/Object
.method public <init>()V
    aload_0
    invokenonvirtual java/lang/Object/<init>()V
    return
.end method
.method public static plus(JJ)J
    .limit stack 10
    .limit locals 10
    lload 0
    lload 2
    ladd
    lreturn
.end method
.method public static main([Ljava/lang/String;)V
    .limit stack 10
    .limit locals 10
    ldc_w "aa"
    astore 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 1
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    ldc2_w 3
    ldc2_w 4
    invokestatic com/example/Example/plus(JJ)J
    lstore 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    lload 1
    invokevirtual java/io/PrintStream/println(J)V
    return
.end method
