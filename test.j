.class public JispCode
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
.method public static f(JJ)J
    .limit stack 10
    .limit locals 10
    lload 0
    ldc2_w 1
    lload 2
    invokestatic JispCode/plus(JJ)J
    invokestatic JispCode/plus(JJ)J
    lreturn
.end method
.method public static main([Ljava/lang/String;)V
    .limit stack 10
    .limit locals 10
    ldc2_w 1
    ldc2_w 2
    invokestatic JispCode/f(JJ)J
    lstore 0
    lload 0
    ldc2_w 1
    invokestatic JispCode/f(JJ)J
    lstore 4
    getstatic java/lang/System/out Ljava/io/PrintStream;
    lload 4
    invokevirtual java/io/PrintStream/println(J)V
    return
.end method