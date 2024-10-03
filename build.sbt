name := "obsidian"
version := "0.0.2"

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

// for publishing to github
ThisBuild / organization := "obsidian.lang.java"
ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := Some(Resolver.file("mavenLocal",  new File(Path.userHome.absolutePath+"/obsidian-java/binrepo/"))) 

//publishArtifact in Test := false
Test / publishArtifact := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/obsidian-java/obsidian</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:obsidian-java/obsidian.git</url>
    <connection>scm:git:git@github.com:obsidian-java/obsidian.git</connection>
  </scm>
  <developers>
    <developer>
      <id>luzhuomi</id>
      <name>Kenny Zhuo Ming Lu</name>
      <url>http://sites.google.com/site/luzhuomi</url>
    </developer>
    <developer>
      <id>Chingles2404</id>
      <name>CCH</name>
      <url></url>
    </developer>
  </developers>)

// end publishing to github 

lazy val root = project.in(file("."))
  .aggregate(obsidian.js, obsidian.jvm)
  .settings(
    publish := {},
    publishLocal := {},

    moduleName := "obsidian",
    libraryDependencies ++= Seq("com.lihaoyi" %%% "utest" % "0.8.2" % "test"),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val obsidian = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "obsidian",
    version := "0.0.2",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.scalactic" %%% "scalactic" % "3.2.9",
      "org.scalatest" %%% "scalatest" % "3.2.9" % "test",
      "org.scala-lang" %%% "toolkit" % "0.1.7",
      "org.typelevel" %%% "cats-core" % "2.10.0",
      "obsidian.lang.java" %%% "scalangj" % "0.1.8"
    ),
  ).
  jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
