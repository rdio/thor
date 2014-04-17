import AssemblyKeys._

assemblySettings

outputPath in assembly := new java.io.File("thor.jar")

mainClass in assembly := Some("com.rdio.thor.Thor")

name := "thor"

organization := "com.rdio"

version := "0.1"

scalaVersion := "2.10.2"

sbtVersion := "0.13.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-target:jvm-1.7"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/.m2/repository")))

resolvers ++= Seq(
  "Twitter Maven repo" at "http://maven.twttr.com/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" % "scrimage-core_2.10" % "1.3.5" exclude("org.slf4j", "slf4j-log4j12"),
  "com.sksamuel.scrimage" % "scrimage-filters_2.10" % "1.3.5" exclude("org.slf4j", "slf4j-log4j12"),
  "com.twitter" %% "finagle-http" % "6.6.2",
  "com.typesafe" % "config" % "1.0.2",
  "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"
)

org.scalastyle.sbt.ScalastylePlugin.Settings