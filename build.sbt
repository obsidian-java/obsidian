name := "obsidian"

version := "0.1.0"

scalaVersion := "3.3.1"

sbtVersion in Global := "1.9.7"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"

// resolvers += "obsidian binary github repo" at "https://raw.githubusercontent.com/luzhuomi/mavenrepo/master/"
resolvers += "obsidian binary github repo" at "https://raw.githubusercontent.com/obsidian-java/binrepo/master/"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

// libraryDependencies += "com.github.luzhuomi" %% "scalangj" % "0.1.5"  // apache license

libraryDependencies += "obsidian.lang.java" %% "scalangj" % "0.1.5"  // apache license

// scalacOptions ++= Seq("-feature", "-deprecation", "-Yresolve-term-conflict:package", "-Ypartial-unification" )


scalacOptions ++= Seq("-source:future") // solve the withFilter is not a member error. 
// https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953/2 

// solution for the same problem in scala 2
// addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0") // to solve the withFilter is not a member of ... error in Scala 2.
// reference https://github.com/oleg-py/better-monadic-for#destructuring-either--io--task--flatmapf
// we don't need this plugin once we migrate to scala 3