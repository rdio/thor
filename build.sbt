import AssemblyKeys._

assemblySettings

outputPath in assembly := new java.io.File("thor.jar")

mainClass in assembly := Some("com.rdio.thor.Thor")

excludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter {_.data.getName == "scalatest_2.10-1.9.1.jar"}
}

name := "thor"

organization := "com.rdio"

version := "0.1"

scalaVersion := "2.10.2"

sbtVersion := "0.13.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-target:jvm-1.7"
)

resolvers ++= Seq(
  "Twitter Maven repo" at "http://maven.twttr.com/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" %% "scrimage-core" % "1.3.5",
  "com.sksamuel.scrimage" %% "scrimage-filters" % "1.3.5",
  "com.twitter" %% "finagle-http" % "6.6.2",
  "com.typesafe" % "config" % "1.2.1",
  "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test"
)

org.scalastyle.sbt.ScalastylePlugin.Settings
