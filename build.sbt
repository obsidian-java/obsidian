name := "obsidian"

version := "0.1.0"


sbtVersion in Global := "1.3.12"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"

resolvers += "luzhuomi github repo" at "https://raw.githubusercontent.com/luzhuomi/mavenrepo/master/"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "com.github.luzhuomi" %% "scalangj" % "0.1.5"  // apache license

scalacOptions ++= Seq("-feature", "-deprecation", "-Yresolve-term-conflict:package", "-Ypartial-unification" )


addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0") // to solve the withFilter is not a member of ... error in Scala 2.
// reference https://github.com/oleg-py/better-monadic-for#destructuring-either--io--task--flatmapf
// we don't need this plugin once we migrate to scala 3