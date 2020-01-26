import sbt._
import Keys._
import Dependencies._

object Core {
  lazy val core = (project in file("core"))
    .settings(Base.settings)
    .settings(
      name := "jisp-core",
      scalacOptions ++= Base.commonScalaOptions,
      addCompilerPlugin(
        "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
      ),
      libraryDependencies ++= Base.commonLibs ++ antlr ++ Seq(
        "org.atnos" %% "eff" % "5.5.2"
      )
    )
}
