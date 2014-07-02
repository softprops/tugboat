organization := "me.lessis"

name := "tugboat"

version := "0.1.0-SNAPSHOT"

scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.1",
  "org.kamranzafar" % "jtar" % "2.2")

initialCommands := "import scala.concurrent.ExecutionContext.Implicits.global; val tb = tugboat.Client()"

seq(buildInfoSettings:_*)

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](version)

buildInfoPackage := "tugboat"
