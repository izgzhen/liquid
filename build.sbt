name := "liquid"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += Resolver.mavenLocal

libraryDependencies += "commons-cli" % "commons-cli" % "1.4"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test" // for running "sbt test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.0"
libraryDependencies += "junit" % "junit" % "4.8.1" % "test"
libraryDependencies += "org.yaml" % "snakeyaml" % "1.25"
libraryDependencies += "info.debatty" % "java-string-similarity" % "1.2.1"
libraryDependencies += "io.github.izgzhen" %% "msbase" % "0.1"

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}

test in assembly := {}

mainClass := Some("org.uwplse.liquid.Main")