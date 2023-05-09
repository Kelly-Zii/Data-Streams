val scala3Version = "3.2.2"
val globalVersion = "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "project",
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.15",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % "test",
      "com.github.nscala-time" %% "nscala-time" % "2.32.0",
      "com.lihaoyi" %% "cask" % "0.8.3",
      "org.postgresql" % "postgresql" % "42.5.4",
      "com.typesafe" % "config" % "1.4.2"
    )
  )

val sjsName = "sjs"
lazy val sjs = project
  .in(file(s"./$sjsName"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := sjsName,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "2.2.0"
    )
  )
