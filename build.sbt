organization := "me.lessis"

name := "tugboat"

version := "0.1.0-SNAPSHOT"

description := "a small boat that maneuvers docker vessels"

crossScalaVersions ++= Seq("2.10.4", "2.11.2")

scalaVersion := crossScalaVersions.value.last

scalacOptions in ThisBuild ++= Seq(Opts.compile.deprecation) ++
  Seq("-Ywarn-unused-import", "-Ywarn-unused", "-Xlint", "-feature").filter(
    Function.const(scalaVersion.value.startsWith("2.11")))

libraryDependencies ++= Seq(
  "org.bouncycastle" % "bcprov-jdk16" % "1.46",
//  "org.bouncycastle" % "bcpg-jdk15on" % "1.51",
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.2",
  "org.kamranzafar" % "jtar" % "2.2")

initialCommands := "import scala.concurrent.ExecutionContext.Implicits.global; val docker = tugboat.Docker()"

seq(buildInfoSettings:_*)

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](version)

buildInfoPackage := "tugboat"
