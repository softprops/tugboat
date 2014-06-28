package tugboat

import org.json4s.native.JsonMethods.parse
import org.json4s._

sealed trait BuildOutput

object BuildOutput {
  case class Progress(message: String) extends BuildOutput
  case class Error(message: String, code: Int, details: String)
    extends BuildOutput
}

/** type class for default representations of streamed output */
sealed trait StreamRep[T] {
  def map: String => T
}

object StreamRep {
  implicit object Identity extends StreamRep[String] {
    def map = identity(_)
  }

  implicit object Nada extends StreamRep[Unit] {
    def map = _ => ()
  }

  implicit object BuildOutputs extends StreamRep[BuildOutput] {
    def map = { str =>
      val JObject(obj) = parse(str)

      def progress = (for {
        ("stream", JString(msg)) <- obj
      } yield BuildOutput.Progress(msg)).headOption

      def err = (for {
        ("error", JString(msg))          <- obj
        ("errorDetail", JObject(detail)) <- obj
        ("code", JInt(code))             <- detail
        ("message", JString(details))    <- detail
      } yield BuildOutput.Error(
        msg, code.toInt, details)).headOption

      progress.orElse(err).get
    } 
  }
}
