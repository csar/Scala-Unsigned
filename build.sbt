lazy val `Scala-Unsigned` = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)


val Version = "0.1.0"

version := "0.1.0"

organization := "net.karana"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))