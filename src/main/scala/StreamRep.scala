package tugboat

import org.json4s.native.JsonMethods.parse
import org.json4s._

object Build {
  sealed trait Output
  case class Progress(message: String) extends Output
  case class Error(message: String, code: Int, details: String) extends Output
}

object Pull {
  sealed trait Output
  case class Status(message: String) extends Output
  case class ProgressDetail(current: Long, total: Long, start: Long, bar: String)
  /** download progress. details will be None in the case the image has already been downloaded */
  case class Progress(message: String, id: String, details: Option[ProgressDetail]) extends Output
  case class Error(message: String, details: String) extends Output 
}

object Push {
  sealed trait Output
  case class Status(message: String) extends Output
  case class Error(message: String, defaults: String) extends Output
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

  implicit object BuildOutputs extends StreamRep[Build.Output] {
    def map = { str =>
      val JObject(obj) = parse(str)

      def progress = (for {
        ("stream", JString(msg)) <- obj
      } yield Build.Progress(msg)).headOption

      def err = (for {
        ("error", JString(msg))          <- obj
        ("errorDetail", JObject(detail)) <- obj
        ("code", JInt(code))             <- detail
        ("message", JString(details))    <- detail
      } yield Build.Error(
        msg, code.toInt, details)).headOption

      progress.orElse(err).get
    } 
  }

  implicit object PushOutputs extends StreamRep[Push.Output] {
    def map = { str =>
      val JObject(obj) = parse(str)
      def status = (for {
        ("status", JString(msg)) <- obj
      } yield Push.Status(msg)).headOption

      def err = (for {
        ("error", JString(msg))          <- obj
        ("errorDetail", JObject(detail)) <- obj
        ("message", JString(details))    <- detail
      } yield Push.Error(
        msg, details)).headOption

      status.orElse(err).get
    }
  }

  implicit object PullOutputs extends StreamRep[Pull.Output] {
    def map = { str =>
      val JObject(obj) = parse(str)

      def progress = (for {
        ("status", JString(message))         <- obj
        ("id", JString(id))                  <- obj
        ("progressDetail", JObject(details)) <- obj
      } yield Pull.Progress(message, id, (for {
        ("current", JInt(current)) <- details
        ("total", JInt(total))     <- details
        ("start", JInt(start))     <- details
        ("progress", JString(bar)) <- obj
      } yield Pull.ProgressDetail(
        current.toLong, total.toLong, start.toLong, bar)).headOption)).headOption

      def status = (for {
        ("status", JString(msg)) <- obj
      } yield Pull.Status(msg)).headOption

      def err = (for {
        ("error", JString(msg))          <- obj
        ("errorDetail", JObject(detail)) <- obj
        ("message", JString(details))    <- detail
      } yield Pull.Error(
        msg, details)).headOption

      progress.orElse(status).orElse(err).get
    }
  }
}
