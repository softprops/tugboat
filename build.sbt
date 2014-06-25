organization := "me.lessis"

name := "tugboat"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq("net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.1")

initialCommands := "import scala.concurrent.ExecutionContext.Implicits.global"
