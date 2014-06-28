package tugboat

import dispatch.{ as, Req }
import java.io.File
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
      _sizes: Option[Boolean] = None) extends Client.Completion[List[tugboat.Container]] {
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
                 _sizes.map(("size" -> _.toString))))(handler)
    }

    case class Create() extends Client.Completion[Unit] {
      def apply[T](handler: Client.Handler[T]) =
        request(base.POST / "create")(handler)
    }

    case class Container(id: String) extends Client.Completion[Option[ContainerDetails]] {
      // todo host config
      case class Start() extends Client.Completion[Unit] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "start")(handler)
      }
      case class Logs() extends Client.Completion[Unit] {
        def apply[T](handler: Client.Handler[T]) =
          request(base / id / "logs")(handler)
      }

      def apply[T](handler: Client.Handler[T]) =
        request(base / id / "json")(handler)

      def top(args: String = "") = new Client.Completion[Top] {
        def apply[T](handler: Client.Handler[T]) =
          request(base / id / "top" <<? Map("ps_args" -> args))(handler)
      }
      
      def logs = Logs()

      def changes = new Client.Completion[List[Change]] {
        def apply[T](handler: Client.Handler[T]) =
          request(base / id / "changes")(handler)
      }
      
      def export[T](handler: Client.Handler[T]) =
        request(base / id / "export")(handler)

      def start = Start()

      def stop[T](after: Int = 0) = new Client.Completion[Unit] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "stop" <<? Map("t" -> after.toString))(handler)
      }

      def restart[T](after: Int = 0) = new Client.Completion[Unit] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "restart" <<? Map("t" -> after.toString))(handler)
      }

      // todo sig
      def kill = new Client.Completion[Unit] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "restart")(handler)
      }

      // todo multiple std in/out
      def attach[T](handler: Client.Handler[T]) =
        request(base.POST / id / "attach")(handler)

      // await -> wait
      def await = new Client.Completion[Status] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "wait")(handler)
      }

      def delete[T](handler: Client.Handler[T]) =
        request(base.DELETE / id)(handler)

      def cp(resource: String) = new Client.Completion[Stream] {
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "copy")(handler)
      }
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
      def all = copy(_all = Some(true))
      def apply[T](handler: Client.Handler[T]) =
        request(base / "json" <<?
                (Map.empty[String, String]
                 ++ _all.map(("all" -> _.toString))))(handler)
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.12/#create-an-image */
    case class Create(
      _fromImage: Option[String] = None,
      _fromSrc: Option[String]   = None,
      _repo: Option[String]      = None,
      _tag: Option[String]       = None,
      _registry: Option[String]  = None) extends Client.Completion[Unit] {
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

    case class Image(id: String) extends Client.Completion[Option[ImageDetails]] {
      case class Push(_registry: Option[String] = None) extends Client.Completion[Unit] {
        def registry(reg: String) = copy(_registry = Some(reg))
        def apply[T](handler: Client.Handler[T]) =
          request(base.POST / id / "push" <<?
                (Map.empty[String, String]
                 ++ _registry.map(("registry" -> _))))(handler)
      }

      case class Tag(
        _repo: Option[String]   = None,
        _force: Option[Boolean] = None) extends Client.Completion[Unit] {
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
        def force(f: Boolean) = copy(_force = Some(f))
        def noprune(np: Boolean) = copy(_noprune = Some(np))
        def apply[T](handler: Client.Handler[T]) =
          request(base.DELETE / id <<?
                  (Map.empty[String, String]
                   ++ _force.map(("force" -> _.toString))
                   ++ _noprune.map(("noprune" -> _.toString))))(handler)
      }

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
      def term(t: String) = copy(_term = Some(t))
      def apply[T](handler: Client.Handler[T]) =
        request(base / "search" <<? _term.map(("term" -> _)))(handler)
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.12/#build-an-image-from-dockerfile-via-stdin */
    case class Build(
      dir: File,
      _tag: Option[String]      = None,
      _q: Option[Boolean]       = None,
      _nocache: Option[Boolean] = None,
      _rm: Option[Boolean]      = None,
      _forcerm: Option[Boolean] = None) extends Client.Stream[BuildOutput] {
      def tag(t: String) = copy(_tag = Some(t))
      def verbose(v: Boolean) = copy(_q = Some(!v))
      def nocache(n: Boolean) = copy(_nocache = Some(n))
      def rm(r: Boolean) = copy(_rm = Some(r))
      def forceRm(r: Boolean) = copy(_forcerm = Some(r))
      def apply[T](handler: Client.Handler[T]) =
        request((host.POST <:< Map(
                "Content-Type" -> "application/x-tar",
                "Content-Encoding" -> "gzip") <<?
                (Map.empty[String, String]
                 ++ _tag.map(("t" -> _))
                 ++ _q.map(("q" -> _.toString))
                 ++ _nocache.map(("nocache" -> _.toString))
                 ++ _rm.map(("rm" -> _.toString))
                 ++ _forcerm.map(("forcerm" -> _.toString)))
                <<< dir) / "build")(handler)
    }

    def list = Images()
    def create = Create()
    def get(id: String) = Image(id)
    def search = Search()
    def build(dir: File) = Build(dir)
  }
}
