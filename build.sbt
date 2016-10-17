lazy val `Scala-Unsigned` = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)


val Version = "0.1.3"

version := "0.1.3"

organization := "net.karana"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))