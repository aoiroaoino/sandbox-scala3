ThisBuild / organization := "dev.aoiroaoino"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.0.0"

lazy val root = (project in file("."))
  .aggregate(sandbox, dividezero)
  .dependsOn(sandbox, dividezero)

lazy val sandbox = project
  .settings(scalacOptions := {
    val dividezeroJar = (dividezero / Compile / packageBin).value.getAbsolutePath
    Seq(
      "-Xlint",
      s"-Xplugin:$dividezeroJar"
    )
  })
  .aggregate(dividezero)

// compiler plugins

lazy val dividezero = (project in file("dividezero"))
  .settings(moduleName := "sandbox-dividezero")
  .settings(libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value)
