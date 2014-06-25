package tugboat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

case class Image(
  id: String, created: Long, size: Long, virtualSize: Long,
  repoTags: List[String] = Nil, parentId: Option[String] = None)

case class SearchResult(
  name: String, description: String, trusted: Boolean, official: Boolean, stars: Int)

/** type class for default representations */
sealed trait Rep[T] {
  def lift: Response => T
}

object Rep {
  implicit object Identity extends Rep[Response] {
    def lift = identity(_)
  }
  implicit object Nada extends Rep[Unit] {
    def lift = _ => ()
  }

  implicit object ListOfImages extends Rep[List[Image]] {
    def lift = (as.json4s.Json andThen (for {
        JArray(imgs)                 <- _
        JObject(img)                 <- imgs
        ("Id", JString(id))          <- img
        ("Created", JInt(created))   <- img
        ("Size", JInt(size))         <- img
        ("VirtualSize", JInt(vsize)) <- img
      } yield tugboat.Image(
        id, created.toLong, size.toLong, vsize.toLong)))
  }

  implicit object ListOfSearchResults extends Rep[List[SearchResult]] {
    def lift = (as.json4s.Json andThen (for {
        JArray(results)                <- _
        JObject(res)                   <- results
        ("name", JString(name))        <- res
        ("description", JString(desc)) <- res
        ("is_trusted", JBool(trust))  <- res
        ("is_official", JBool(offic))   <- res
        ("star_count", JInt(stars))    <- res
      } yield SearchResult(
        name, desc, trust, offic, stars.toInt)))
  }
}
