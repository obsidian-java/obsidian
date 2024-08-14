name := "obsidian"
version := "0.1.0"

Global / sbtVersion := "1.9.7"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
ThisBuild / resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"
ThisBuild / resolvers += "clojars" at "https://clojars.org/repo"
ThisBuild / resolvers += "obsidian binary github repo" at "https://raw.githubusercontent.com/obsidian-java/binrepo/master/"

ThisBuild / scalacOptions ++= Seq("-source:future") // solve the withFilter is not a member error. 
// https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953/2 

// solution for the same problem in scala 2
// addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0") // to solve the withFilter is not a member of ... error in Scala 2.
// reference https://github.com/oleg-py/better-monadic-for#destructuring-either--io--task--flatmapf
// we don't need this plugin once we migrate to scala 3

import scala.sys.process._

lazy val root = project.in(file("."))
  .aggregate(obsidian.js, obsidian.jvm)
  .settings(
    publish := {},
    publishLocal := {},

    moduleName := "obsidian",

    Compile / fastOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value / "out" / "extension.js",
    install := installDependenciesTask.dependsOn(Compile / fastOptJS).value,
    libraryDependencies ++= Seq("com.lihaoyi" %%% "utest" % "0.8.2" % "test"),
    Compile / npmDependencies ++= Seq("vscode" -> "1.84.1"),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(
    ScalablyTypedConverterPlugin
  )

  lazy val obsidian = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "obsidian",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.scalactic" %%% "scalactic" % "3.2.9",
      "org.scalatest" %%% "scalatest" % "3.2.9" % "test",
      "org.scala-lang" %%% "toolkit" % "0.1.7",
      "org.typelevel" %%% "cats-core" % "2.10.0",
      "obsidian.lang.java" %%% "scalangj" % "0.1.8"
    ),
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    Compile / npmDependencies ++= Seq("vscode" -> "1.84.1"),
    install := installDependenciesTask.dependsOn(Compile / fastOptJS).value,
    Compile / fastOptJS / artifactPath := (ThisBuild / baseDirectory).value / "out" / "extension.js",
    Compile / fullOptJS / artifactPath := (ThisBuild / baseDirectory).value / "out" / "extension.js",
  )
  .enablePlugins(
    ScalaJSBundlerPlugin,
    ScalablyTypedConverterPlugin
  )

lazy val install = taskKey[Unit]("install dependencies")
def installDependenciesTask: Def.Initialize[Task[Unit]] =
  Def
    .task[Unit] {
      val base = (ThisProject / baseDirectory).value
      val log = (ThisProject / streams).value.log
      if (!(base / "node_module").exists) {
        val pb =
          new java.lang.ProcessBuilder("npm", "install")
            .directory(base)
            .redirectErrorStream(true)

        pb ! log
      }
    }