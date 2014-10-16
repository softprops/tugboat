package tugboat

import org.json4s.native.JsonMethods.parse
import org.json4s._

object Event {
  case class Status(status: String, id: String, from: String, time: Long)  
}

object Build {
  sealed trait Output
  case class Progress(message: String) extends Output
  case class Status(message: String) extends Output
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
  implicit val Identity: StreamRep[String] =
    new StreamRep[String] {
      def map = identity(_)
    }

  implicit val Nada: StreamRep[Unit] =
    new StreamRep[Unit] {
      def map = _ => ()
    }

  implicit val StreamEvents: StreamRep[Event.Status] =
    new StreamRep[Event.Status] {
      def map = { str =>
        (for {
          JObject(event)              <- parse(str)
          ("status", JString(status)) <- event
          ("id", JString(id))         <- event
          ("from", JString(from))     <- event
          ("time", JInt(time))        <- event
        } yield Event.Status(status, id, from, time.toLong)).head
      }
    }

  implicit val BuildOutputs: StreamRep[Build.Output] = new StreamRep[Build.Output] {
    def map = { str =>
      val JObject(obj) = parse(str)

      def status = (for {
        ("status", JString(status)) <- obj
      } yield Build.Status(status)).headOption

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

      progress.orElse(status).orElse(err).get
    } 
  }

  implicit val PushOutputs: StreamRep[Push.Output] = new StreamRep[Push.Output] {
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
