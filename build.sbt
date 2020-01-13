organization in ThisBuild := "me.kerfume"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "0.1.0-SNAPSHOT"
scalafmtOnCompile in ThisBuild := true
resolvers in ThisBuild += "kerfume-util repository" at "https://keisunagawa.github.io/kerfume-scala-util/repo/"

lazy val core = Core.core
lazy val root = (project in file("."))
  .aggregate(core)
