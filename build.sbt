lazy val `Scala-Unsigned` = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)

version := "0.1.0"
  
libraryDependencies ++= Vector(
  Library.scalaTest % "test"
)

libraryDependencies += "junit" % "junit" % "4.10" % "test"