package tugboat

import dispatch.{ as, Req }
import org.json4s.JsonDSL._
import org.json4s.{ JArray, JBool, JInt, JObject, JString, JValue }
import org.json4s.native.JsonMethods.render
import org.json4s.native.Printer.compact
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait Methods { self: Requests =>

  private object json {
    private[this] val Typ = "application/json"
    private[this] val Encoding = "UTF-8"
    def content(r: Req) = r.setContentType(Typ, Encoding)
    def str(jv: JValue) = compact(render(jv))
  }

  object containers {
    private[this] def base = host / "containers"

    case class Containers(
      _all: Option[Boolean]           = None,
      _limit: Option[Int]             = None,
      _since: Option[FiniteDuration]  = None,
      _before: Option[FiniteDuration] = None,
      _sizes: Option[Boolean] = None) extends Client.Completion[Unit] {
      def asRep = _ => ()
      def all = copy(_all = Some(true))
      def limit(lim: Int) = copy(_limit = Some(lim))
      def since(s: FiniteDuration) = copy(_since = Some(s))
      def before(b: FiniteDuration) = copy(_before = Some(b))
      def sizes(include: Boolean) = copy(_sizes = Some(include))
      def apply[T](handler: Client.Handler[T]) =
        request(base / "json" <<?
                (Map.empty[String, String] ++ _all.map(("all" -> _.toString)) ++
                 _limit.map(("limit" -> _.toString)) ++
                 _before.map(("before" -> _.toSeconds.toString)) ++
                 _since.map(("since" -> _.toSeconds.toString)) ++
                 _sizes.map(("sizes" -> _.toString))))(handler)
        
    }

    case class Create() extends Client.Completion[Unit] {
      def asRep = _ => ()
      def apply[T](handler: Client.Handler[T]) =
        request(base.POST / "create")(handler)
      
    }

    case class Container(id: String) extends Client.Completion[Unit] {
      case class Logs() extends Client.Completion[Unit] {
        def asRep = _ => ()
        def apply[T](handler: Client.Handler[T]) =
          request(base / id / "logs")(handler)
      }

      def asRep = _ => ()

      def apply[T](handler: Client.Handler[T]) =
        request(base / id / "json")(handler)

      def top[T](handler: Client.Handler[T]) =
        request(base / id / "top")(handler)
      
      def logs = Logs()

      def changes[T](handler: Client.Handler[T]) =
        request(base / id / "changes")(handler)
      
      def export[T](handler: Client.Handler[T]) =
        request(base / id / "export")(handler)

      def start[T](handler: Client.Handler[T]) =
        request(base.POST / id / "start")(handler)

      def stop[T](after: Int = 0)(handler: Client.Handler[T]) =
        request(base.POST / id / "stop" <<? Map("t" -> after.toString))(handler)

      def restart[T](after: Int = 0)(handler: Client.Handler[T]) =
        request(base.POST / id / "restart" <<? Map("t" -> after.toString))(handler)

      def kill[T](handler: Client.Handler[T]) =
        request(base.POST / id / "restart")(handler)

      def attach[T](handler: Client.Handler[T]) =
        request(base.POST / id / "attach")(handler)

      def wait[T](handler: Client.Handler[T]) =
        request(base.POST / id / "wait")(handler)

      def delete[T](handler: Client.Handler[T]) =
        request(base.DELETE / id)(handler)

      def cp[T](handler: Client.Handler[T]) =
        request(base.POST / id / "copy")(handler)
    }

    def list = Containers()
    def create = Create()
    def get(id: String) = Container(id)
  }

  object images {
    private[this] def base = host / "images"

    case class Images(
      _all: Option[Boolean]   = None,
      _filter: Option[String] = None) extends Client.Completion[List[tugboat.Image]] {

      def asRep = (as.json4s.Json andThen (for {
        JArray(imgs)                 <- _
        JObject(img)                 <- imgs
        ("Id", JString(id))          <- img
        ("Created", JInt(created))   <- img
        ("Size", JInt(size))         <- img
        ("VirtualSize", JInt(vsize)) <- img
      } yield tugboat.Image(
        id, created.toLong, size.toLong, vsize.toLong)))

      def all = copy(_all = Some(true))
      def apply[T](handler: Client.Handler[T]) =
        request(base / "json" <<?
                (Map.empty[String, String]
                 ++ _all.map(("all" -> _.toString))))(handler)
    }

    case class Create(
      _fromImage: Option[String] = None,
      _fromSrc: Option[String]   = None,
      _repo: Option[String]      = None,
      _tag: Option[String]       = None,
      _registry: Option[String]  = None) extends Client.Completion[Unit] {
      def asRep = _ => ()

      def fromImage(img: String) = copy(_fromImage = Some(img))
      def fromSrc(src: String) = copy(_fromSrc = Some(src))
      def repo(r: String) = copy(_repo = Some(r))
      def tag(t: String) = copy(_tag = Some(t))
      def registry(r: String) = copy(_registry = Some(r))      
      def apply[T](handler: Client.Handler[T]) =
        request(base.POST / "create" <<?
              (Map.empty[String, String] ++ _fromImage.map(("fromImage" -> _))
               ++ _fromSrc.map(("fromSrc" -> _))
               ++ _repo.map(("repo" -> _))
               ++ _tag.map(("tag" -> _))
               ++ _registry.map(("registry" -> _))))(handler)
    }

    case class Image(id: String) extends Client.Completion[Unit] {
      case class Push(_registry: Option[String] = None) extends Client.Completion[Unit] {
      def asRep = _ => ()
        def registry(reg: String) = copy(_registry = Some(reg))
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "push" <<?
                (Map.empty[String, String]
                 ++ _registry.map(("registry" -> _))))(handler)
      }

      case class Tag(
        _repo: Option[String]   = None,
        _force: Option[Boolean] = None) extends Client.Completion[Unit] {
        def asRep = _ => ()
        def repo(r: String) = copy(_repo = Some(r))
        def force(f: Boolean) = copy(_force = Some(f))
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "tag" <<?
                (Map.empty[String, String]
                 ++ _repo.map(("repo" -> _))
                 ++ _force.map(("force" -> _.toString))))(handler)
      }

      case class Delete(
        _force: Option[Boolean]   = None,
        _noprune: Option[Boolean] = None) extends Client.Completion[Unit] {
        def asRep = _ => ()
        def force(f: Boolean) = copy(_force = Some(f))
        def noprune(np: Boolean) = copy(_noprune = Some(np))
        def apply[T](handler: Client.Handler[T]) =
          request(base.DELETE / id <<?
                  (Map.empty[String, String]
                   ++ _force.map(("force" -> _.toString))
                   ++ _noprune.map(("noprune" -> _.toString))))(handler)
      }

      def asRep = _ => ()

      def apply[T](handler: Client.Handler[T]) =
        request(base / id / "json")(handler)

      def history = complete(base / id / "history")

      def insert(url: String, path: String) =
        complete(base.POST / id  / "insert" <<?
                 Map("url" -> url, "path" -> path))

      def push = Push()

      def tag = Tag()

      def delete = Delete()
    }

    case class Search(
      _term: Option[String] = None) extends Client.Completion[List[SearchResult]] {

      def asRep = (as.json4s.Json andThen (for {
        JArray(results)                <- _
        JObject(res)                   <- results
        ("name", JString(name))        <- res
        ("description", JString(desc)) <- res
        ("is_trusted", JBool(trust))  <- res
        ("is_official", JBool(offic))   <- res
        ("star_count", JInt(stars))    <- res
      } yield SearchResult(
        name, desc, trust, offic, stars.toInt)))

      def term(t: String) = copy(_term = Some(t))
      def apply[T](handler: Client.Handler[T]) =
        request(base / "search" <<? _term.map(("term" -> _)))(handler)
    }
    
    def list = Images()
    def create = Create()
    def get(id: String) = Image(id)
    def search = Search()
  }
}
