package tugboat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._


case class Port(ip: String, priv: Int, pub: Int, typ: String)

case class Container(
  id: String, image: String, cmd: String, created: Long, status: String,
  ports: Seq[Port], names: Seq[String],
  sizeRw: Option[Long] = None, sizeRootFs: Option[Long] = None)

case class Image(
  id: String, created: Long, size: Long, virtualSize: Long,
  repoTags: List[String] = Nil, parent: Option[String] = None)

case class ImageDetails(
  id: String, created: String, container: String, size: Long, parent: Option[String] = None)

case class SearchResult(
  name: String, description: String, trusted: Boolean, official: Boolean, stars: Int)

/** type class for default representations */
sealed trait Rep[T] {
  def map: Response => T
}

object Rep {
  implicit object Identity extends Rep[Response] {
    def map = identity(_)
  }

  implicit object Nada extends Rep[Unit] {
    def map = _ => ()
  }

  implicit object ListOfContainers extends Rep[List[Container]] {
    def map = (as.json4s.Json andThen (for {
      JArray(containers)         <- _
      JObject(cont)              <- containers
      ("Id", JString(id))        <- cont
      ("Image", JString(img))    <- cont
      ("Command", JString(cmd))  <- cont
      ("Created", JInt(created)) <- cont
      ("Status", JString(stat))  <- cont
    } yield Container(
      id, img, cmd, created.toLong, stat,
      for {
        ("Ports", JArray(ps))       <- cont
        JObject(port)               <- ps
        ("IP", JString(ip))         <- port
        ("PrivatePort", JInt(priv)) <- port
        ("PublicPort", JInt(pub))   <- port
        ("Type", JString(typ))      <- port
      } yield Port(ip, priv.toInt, pub.toInt, typ),
      for {
        ("Names", JArray(names)) <- cont
        JString(name) <- names
      } yield name,
      (for {
        ("SizeRw", JInt(size)) <- cont
      } yield size.toLong).headOption,
      (for {
        ("SizeRootFs", JInt(size)) <- cont
      } yield size.toLong).headOption
    )))
  }

  implicit object ListOfImages extends Rep[List[Image]] {
    def map = (as.json4s.Json andThen (for {
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
    def map = (as.json4s.Json andThen (for {
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

  implicit object ImageDetail extends Rep[Option[ImageDetails]] {
    def map = { r => (for {
      JObject(img) <- as.json4s.Json(r)
      ("Id", JString(id)) <- img
      ("Created", JString(created)) <- img
      ("Container", JString(container)) <- img
      ("Size", JInt(size)) <- img
    } yield ImageDetails(
      id, created, container, size.toLong)).headOption
    }
  }
}
