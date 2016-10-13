lazy val `Scala-Unsigned` = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)


val Version = "0.1.1"

version := "0.1.1"

organization := "net.karana"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))