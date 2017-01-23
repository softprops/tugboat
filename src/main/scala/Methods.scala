package tugboat

import com.ning.http.client.generators.InputStreamBodyGenerator
import dispatch.{ as, Req }
import dispatch.stream.StringsByLine
import java.io.{ File, PipedInputStream, PipedOutputStream, InputStream, OutputStream }
import org.json4s.JsonDSL._
import org.json4s.{ JArray, JBool, JInt, JNull, JObject, JString, JValue, JField }
import org.json4s.native.JsonMethods.{ compact, render }
import scala.concurrent.Future
import scala.concurrent.duration._

trait Methods { self: Requests =>

  private object json {
    private[this] val ContentType = "application/json"
    private[this] val Encoding = "UTF-8"
    def content(r: Req) = r.setContentType(ContentType, Encoding)
    def str(jv: JValue) = compact(render(jv))
  }

  case class Auth(_cfg: AuthConfig)
    extends Docker.Completion[Unit] { // fixme: better rep

    def config(cfg: AuthConfig) = copy(_cfg = cfg)

    def withConfig(f: AuthConfig => AuthConfig) =
        config(f(_cfg))

    def user(u: String) =
      withConfig(_.copy(user = u))

    def password(pw: String) =
      withConfig(_.copy(password = pw))

    def email(em: String) =
      withConfig(_.copy(email = em))

    def server(svr: String) =
      withConfig(_.copy(server = svr))

    def apply[T](handler: Docker.Handler[T]) =
      request(json.content(host / "auth") << body)(handler)

    def body =
      json.str(
        ("username"      -> _cfg.user) ~
        ("password"      -> _cfg.password) ~
        ("email"         -> _cfg.email) ~
        ("serveraddress" -> _cfg.server))
  }

  case class Events(
    _since: Option[Long] = None,
    _until: Option[Long] = None)
    extends Docker.Stream[Event.Record] {
    def since(s: Long) = copy(_since = Some(s))
    def until(u: Long) = copy(_until = Some(u))
    def apply[T](handler: Docker.Handler[T]) =
      request(host / "events" <<? query)(handler)

    def query = (
      Map.empty[String, String]
        ++ _since.map( s => ("since"   -> (s / 1000).toString))
        ++ _until.map( u => ("until"   -> (u / 1000).toString)))

    override protected def streamer =
      Docker.Stream.chunk[Event.Record]
  }

  def version = complete[Version](host / "version")

  def info = complete[Info](host / "info")

  def ping = complete[Unit](host / "_ping")

  def events = Events()

  def auth(user: String, password: String, email: String): Auth =
    auth(AuthConfig(user, password, email))

  def auth(cfg: AuthConfig): Auth = Auth(cfg)

  object containers {
    private[this] def base = host / "containers"

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#list-containers */
    case class Containers(
      private val _all: Option[Boolean]   = None,
      private val _limit: Option[Int]     = None,
      private val _since: Option[String]  = None,
      private val _before: Option[String] = None,
      private val _sizes: Option[Boolean] = None) // todo: filters exited & status
      extends Docker.Completion[List[tugboat.Container]] {
      def all(a: Boolean) = copy(_all = Some(a))
      def limit(lim: Int) = copy(_limit = Some(lim))
      def since(s: String) = copy(_since = Some(s))
      def before(b: String) = copy(_before = Some(b))
      def sizes(include: Boolean) = copy(_sizes = Some(include))
      def apply[T](handler: Docker.Handler[T]) =
        request(base / "json" <<? query)(handler)

      def query = (
        Map.empty[String, String]
          ++ _all.map(("all"       -> _.toString))
          ++ _limit.map(("limit"   -> _.toString))
          ++ _before.map(("before" -> _))
          ++ _since.map(("since"   -> _))
          ++ _sizes.map(("size"    -> _.toString)))
    }    

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#create-a-container */
    case class Create(
      private val _config: ContainerConfig,
      private val _name: Option[String]                 = None,
      private val _restartPolicy: Option[RestartPolicy] = None)
      extends Docker.Completion[tugboat.Create.Response] {

      def name(n: String) = copy(_name = Some(n))

      def config(cfg: ContainerConfig) = copy(_config = cfg)

      def withConfig(f: ContainerConfig => ContainerConfig) =
        config(f(_config))

      /** String value containing the image name to use for the container */
      def image(img: String) =
        withConfig(_.copy(image = img))

      /** Boolean value, attaches to stdin */
      def attachStdin(in: Boolean) =
        withConfig(_.copy(attachStdin = in))

      /** Boolean value, attaches to stdout. */
      def attachStdout(out: Boolean) =
        withConfig(_.copy(attachStdout = out))

      /** Boolean value, attaches to stderr. */
      def attachStderr(err: Boolean) =
        withConfig(_.copy(attachStderr = err))

      /** Command to run specified as a string or an array of strings */
      def cmd(args: String*) =
        withConfig(_.copy(cmd = args.toSeq))

      /** An integer value containing the CPU Shares for container (ie. the relative weight vs othercontainers) */
      def cpuShares(cpu: Int) =
        withConfig(_.copy(cpuShares = cpu))

      /** String value containg the cgroups Cpuset to use */
      def cpuSet(set: String) =
        withConfig(_.copy(cpuSet = set))

      def domainName(name: String) =
        withConfig(_.copy(domainName = name))

      /** Set the entrypoint for the container a a string or an array of strings */
      def entryPoint(ep: String*) =
        withConfig(_.copy(entryPoint = ep.toSeq))

      /** A list of environment variables */
      def env(vars: (String, String)*) =
        withConfig(_.copy(env = vars.toMap))

      // todo: types!
      def exposedPorts(ports: String*) =
        withConfig(_.copy(exposedPorts = ports.toSeq))

      def hostname(name: String) =
        withConfig(_.copy(hostname = name))

      /** Memory limit in bytes */
      def memory(mem: Long) =
        withConfig(_.copy(memory = mem))

      /**  Total memory usage (memory + swap); set -1 to disable swap */
      def memorySwap(swap: Long) =
        withConfig(_.copy(memorySwap = swap))

      /** Boolean value, when true disables neworking for the container */
      def networkDisabled(dis: Boolean) =
        withConfig(_.copy(networkDisabled = dis))

      /** Boolean value, opens stdin */
      def openStdin(in: Boolean) =
        withConfig(_.copy(openStdin = in))

      /**  Boolean value, close stdin after the 1 attached client disconnects */
      def stdinOnce(once: Boolean) =
        withConfig(_.copy(stdinOnce = once))

      def user(u: String) =
        withConfig(_.copy(user = u))

      /**  Boolean value, Attach standard streams to a tty, including stdin if it is not closed. */
      def tty(is: Boolean) =
        withConfig(_.copy(tty = is))

      /** An object mapping mountpoint paths (strings) inside the container to empty objects. */
      def volumes(vx: String*) =
        withConfig(_.copy(volumes = vx.toSeq))

      /**  A string value containing the working dir for commands to run in. */
      def workingDir(dir: String) =
        withConfig(_.copy(workingDir = dir))
        
      def restartPolicy(p: RestartPolicy) = copy(
        _restartPolicy = Some(p)
      )

      def apply[T](handler: Docker.Handler[T]) =
        request(json.content(base.POST) / "create"
                <<? query
                << body)(handler)

      def query = Map.empty[String, String] ++ _name.map(("name" -> _))

      // config https://github.com/dotcloud/docker/blob/master/runconfig/parse.go#L213
      // host config https://github.com/dotcloud/docker/blob/master/runconfig/parse.go#L236
      // run https://github.com/dotcloud/docker/blob/master/api/client/commands.go#L1897

      def body = json.str(
        ("Hostname"        -> _config.hostname) ~
        ("Domainname"      -> _config.domainName) ~
        ("ExposedPorts"    -> JObject(_config.exposedPorts.map { ep =>
          JField(ep, JObject())
        }.toList)) ~
        ("User"            -> _config.user) ~
        ("Tty"             -> _config.tty) ~
        ("NetworkDisabled" -> _config.networkDisabled) ~
        ("OpenStdin"       -> _config.openStdin) ~
        ("Memory"          -> _config.memory) ~
        ("CpuShares"       -> _config.cpuShares) ~
        ("Cpuset"          -> _config.cpuSet) ~
        ("AttachStdin"     -> _config.attachStdin) ~
        ("AttachStdout"    -> _config.attachStdout) ~
        ("AttachStderr"    -> _config.attachStderr) ~
        ("Env"             -> _config.env.map { case (k,v) => s"$k=$v" }) ~
        ("Cmd"             -> Option(_config.cmd).filter(_.nonEmpty)) ~
        ("Image"           -> _config.image) ~
        ("Volumes"         -> JObject(_config.volumes.map { vol =>
          JField(vol, JObject())
        }.toList )) ~
        ("WorkingDir"      -> _config.workingDir) ~
        ("RestartPolicy"   -> _restartPolicy.map { policy =>
          ("Name" -> policy.name)
        }))
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#inspect-a-container */
    case class Container(id: String)
      extends Docker.Completion[ContainerDetails] {

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.15/#start-a-container */
      case class Start(_config: HostConfig)
        extends Docker.Completion[Unit] {

        def config(cfg: HostConfig) = copy(_config = cfg)

        def withConfig(f: HostConfig => HostConfig) =
          config(f(_config))

        def publishAllPorts(pub: Boolean) =
          withConfig(_.copy(publishAllPorts = pub))

        // https://docs.docker.com/userguide/dockerlinks/
        // docker -p  format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort 

        def portBind(containerPort: Port, binding: PortBinding*) =
          withConfig(_.copy(ports = _config.ports + (containerPort -> binding.toList)))

        def volumeBind(bindings: VolumeBinding*) =
          withConfig(_.copy(binds = bindings.toSeq))

        def volumesFrom(bindings: VolumeFromBinding*) =
          withConfig(_.copy(volumesFrom = bindings.toSeq))

        def links(lx: String*) =
          withConfig(_.copy(links = lx.toSeq))

        def capAdd(caps: String*) =
          withConfig(_.copy(capAdd = caps.toSeq))

        def capDrop(caps: String*) =
          withConfig(_.copy(capDrop = caps.toSeq))

        def privileged(priv: Boolean) =
          withConfig(_.copy(privileged = priv))

        def dns(hosts: String*) =
          withConfig(_.copy(dns = hosts.toSeq))

        // todo: complete builder interface
        def apply[T](handler: Docker.Handler[T]) =
          request(json.content(base.POST) / id / "start" << body)(handler)

        def body = json.str(
          ("Binds" -> Option(_config.binds.map(_.spec)).filter(_.nonEmpty)) ~
          ("ContainerIDFile" -> _config.containerIdFile) ~
          ("LxcConf" -> _config.lxcConf) ~
          ("Privileged" -> _config.privileged) ~
          ("PortBindings" -> _config.ports.map {
            case (port, bindings) =>
              (port.spec -> bindings.map { binding =>
                ("HostIp" -> binding.hostIp) ~
                ("HostPort" -> binding.hostPort.toString)
              })
          }) ~
          ("Links" -> Option(_config.links).filter(_.nonEmpty)) ~
          ("PublishAllPorts" -> _config.publishAllPorts) ~
          ("Dns" -> Option(_config.dns).filter(_.nonEmpty)) ~
          ("DnsSearch" -> Option(_config.dnsSearch).filter(_.nonEmpty)) ~
          ("NetworkMode" -> _config.networkMode.value) ~
          ("VolumesFrom" -> Option(_config.volumesFrom.map(_.spec)).filter(_.nonEmpty)) ~
          ("CapAdd" -> Option(_config.capAdd).filter(_.nonEmpty)) ~
          ("CapDrop" -> Option(_config.capDrop).filter(_.nonEmpty)))
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.15/#kill-a-container */
      case class Kill(
        _signal: Option[String] = None)
        extends Docker.Completion[Unit] {

        def signal(sig: String) = copy(_signal = Some(sig))

        def apply[T](handler: Docker.Handler[T]) =
          request(base.POST / id / "kill"
                  <<? query)(handler)

         def query = Map.empty[String, String] ++ _signal.map(("signal" -> _))
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#get-container-logs */
      case class Logs(
        private val _follow: Option[Boolean]     = None,
        private val _stdout: Option[Boolean]     = None,
        private val _stderr: Option[Boolean]     = None,
        private val _timestamps: Option[Boolean] = None,
        private val _tail: Option[String]        = None)
        extends Docker.Stream[String] {

        def stdout(b: Boolean) = copy(_stdout = Some(b))

        def stderr(b: Boolean) = copy(_stderr = Some(b))

        def timestamps(ts: Boolean) = copy(_timestamps = Some(ts))

        def follow(fol: Boolean) = copy(_follow = Some(fol))

        def tail(lines: Int) = copy(_tail = Some(lines.toString))

        def tailAll = copy(_tail = Some("all"))

        def apply[T](handler: Docker.Handler[T]) =
          request(base / id / "logs" <<? query)(handler)

         def query = (
           Map.empty[String, String]
             ++ _follow.map(("follow" -> _.toString))
             ++ _stdout.map(("stdout" -> _.toString))
             ++ _stderr.map(("stderr" -> _.toString))
             ++ _timestamps.map(("timestamps" -> _.toString))
             ++ _tail.map(("tail" -> _)))
      }

      case class Delete(
        private val _volumes: Option[Boolean] = None,
        private val _force: Option[Boolean]   = None)
        extends Docker.Completion[Unit] {
        def volumes(v: Boolean) = copy(_volumes = Some(v))
        def force(f: Boolean) = copy(_force = Some(f))
        def apply[T](handler: Docker.Handler[T]) =
          request(base.DELETE / id <<? query)(handler)

        def query = (
          Map.empty[String, String]
            ++ _volumes.map(("v" -> _.toString))
            ++ _force.map(("force" -> _.toString)))
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.15/#inspect-a-container */
      def apply[T](handler: Docker.Handler[T]) =
        request(base / id / "json")(handler)

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#list-processes-running-inside-a-container */
      def top(args: String = "") =
        complete[Top](base / id / "top" <<? Map("ps_args" -> args))
      
      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#get-container-logs */
      def logs = Logs()

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#inspect-changes-on-a-containers-filesystem */
      def changes =
        complete[List[Change]](base / id / "changes")
      
      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#export-a-container */
      def export(toFile: File) =
        request(base / id / "export")(dispatch.as.File(toFile))

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#start-a-container */
      def start = Start(HostConfig())

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#stop-a-container */
      def stop(after: FiniteDuration = 0.seconds) =
        complete[Unit](base.POST / id / "stop" <<? Map("t" -> after.toString))

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#restart-a-container */
      def restart(after: FiniteDuration = 0.seconds) =
        complete[Unit](base.POST / id / "restart" <<? Map("t" -> after.toString))

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#kill-a-container */
      def kill = Kill()

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#pause-a-container */
      def pause =
        complete[Unit](base.POST / id / "pause")

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#unpause-a-container */
      def unpause =
        complete[Unit](base.POST / id / "unpause")

      // todo multiple std in/out
      case class Attach(
        private val _logs: Option[Boolean]   = None,
        private val _stream: Option[Boolean] = None,
        private val _stdin: Option[Boolean]  = None,
        private val _stdout: Option[Boolean] = None,
        private val _stderr: Option[Boolean] = None) {
        def logs(l: Boolean) = copy(_logs = Some(l))
        def stream(s: Boolean) = copy(_stream = Some(s))
        def stdin(s: Boolean) = copy(_stdin = Some(s))
        def stdout(s: Boolean) = copy(_stdout = Some(s))
        def stderr(s: Boolean) = copy(_stderr = Some(s))

        private def req =
          base.POST / id / "attach" <<? query

         def query = (
           Map.empty[String, String]
             ++ _logs.map(("logs"     -> _.toString))
             ++ _stream.map(("stream" -> _.toString))
             ++ _stdin.map(("stdin"   -> _.toString))
             ++ _stdout.map(("stdout" -> _.toString))
             ++ _stderr.map(("stderr" -> _.toString)))

        /** todo: consider processIO https://github.com/scala/scala/blob/v2.11.2/src/library/scala/sys/process/ProcessIO.scala#L1 */
        def apply(in: OutputStream => Unit, out: String => Unit = _ => ()) = {
          val os = new PipedOutputStream()
          val is = new PipedInputStream(os)
          in(os)
          request(req.subject.underlying(_.setBody(new InputStreamBodyGenerator(is))))(
            new StringsByLine[Unit] with Docker.StreamErrorHandler[Unit]
              with Docker.Stream.Stopper {
              def onStringBy(str: String) = out(str)
              def onCompleted = ()
            })
        }
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.15/#attach-to-a-container */
      //def attach = Attach()

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#wait-a-container */
      def await =
        complete[Status](base.POST / id / "wait")

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#remove-a-container */
      def delete = Delete()

      // todo: octet stream
      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#copy-files-or-folders-from-a-container */
     /* def cp(resource: String) = new Docker.Stream[Unit] {
        def apply[T](handler: Docker.Handler[T]) =
          request(base.POST / id / "copy")(handler)
      }*/
    }

    /** aka docker ps */
    def list = Containers()

    /** aka docker run */
    def create(image: String) = Create(ContainerConfig(image))

    /** aka docker inspect */
    def get(id: String) = Container(id)
  }

  object images {
    private[this] def base = host / "images"

    case class Images(
      private val _all: Option[Boolean]                       = None,
      private val _filters: Option[Map[String, List[String]]] =  None)
      extends Docker.Completion[List[tugboat.Image]] {
      def all = copy(_all = Some(true))
      def filters(fs: Map[String, List[String]]) = copy(_filters = Some(fs))
      // ( aka untagged ) for convenience
      def dangling(dang: Boolean) = filters(Map("dangling" -> (dang.toString :: Nil)))

      def apply[T](handler: Docker.Handler[T]) =
        request(base / "json" <<? query)(handler)

      def query = (
        Map.empty[String, String]
          ++ _all.map(("all" -> _.toString))
          ++ _filters.map("filters" -> json.str(_)))
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#create-an-image */
    case class Pull(
      private val _fromImage: String,
      private val _fromSrc: Option[String]   = None,
      private val _repo: Option[String]      = None,
      private val _tag: Option[String]       = None,
      private val _registry: Option[String]  = None)
      extends Docker.Stream[tugboat.Pull.Output] {
      override protected def streamer =
        Docker.Stream.chunk[tugboat.Pull.Output]

      def fromImage(img: String) = copy(_fromImage = img)
      def fromSrc(src: String) = copy(_fromSrc = Some(src))
      def repo(r: String) = copy(_repo = Some(r))
      // if fromImage includes a tag, foobar:tag, this is not needed
      def tag(t: String) = copy(_tag = Some(t))
      def registry(r: String) = copy(_registry = Some(r))      
      def apply[T](handler: Docker.Handler[T]) =
        request(base.POST / "create" <:< authConfig.map(
                  ("X-Registry-Auth" -> _.headerValue)
                 ) <<? query)(handler)

       def query = (
         Map("fromImage" -> _fromImage)
           ++ _fromSrc.map(("fromSrc" -> _))
           ++ _repo.map(("repo" -> _))
           ++ _tag.map(("tag" -> _))
           ++ _registry.map(("registry" -> _)))
    }

    case class Image(id: String)
      extends Docker.Completion[Option[ImageDetails]] {

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#push-an-image-on-the-registry */
      case class Push(
        _registry: Option[String] = None)
        extends Docker.Stream[tugboat.Push.Output] {
        def registry(reg: String) = copy(_registry = Some(reg))
        def apply[T](handler: Docker.Handler[T]) =
          request(base.POST / id / "push" <:<
                 (Map.empty[String, String]
                  ++ authConfig.map(
                    ("X-Registry-Auth" -> _.headerValue)
                  )) <<?
                 (Map.empty[String, String]
                  ++ _registry.map(("registry" -> _))))(handler)
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#tag-an-image-into-a-repository */
      case class Tag(
        _repo: Option[String]   = None,
        _force: Option[Boolean] = None)
        extends Docker.Completion[Unit] {
        def repo(r: String) = copy(_repo = Some(r))
        def force(f: Boolean) = copy(_force = Some(f))
        def apply[T](handler: Docker.Handler[T]) =
          request(base.POST / id / "tag" <<? query)(handler)

        def query = (
          Map.empty[String, String]
            ++ _repo.map(("repo" -> _))
            ++ _force.map(("force" -> _.toString)))
      }
       
      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#remove-an-image */
      case class Delete(
        private val _force: Option[Boolean]   = None,
        private val _noprune: Option[Boolean] = None)
        extends Docker.Completion[List[ImageDeletionStatus]] {
        def force(f: Boolean) = copy(_force = Some(f))
        def noprune(np: Boolean) = copy(_noprune = Some(np))
        def apply[T](handler: Docker.Handler[T]) =
          request(base.DELETE / id <<? query)(handler)

        def query = (
          Map.empty[String, String]
            ++ _force.map(("force" -> _.toString))
            ++ _noprune.map(("noprune" -> _.toString)))
      }

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#inspect-an-image */
      def apply[T](handler: Docker.Handler[T]) =
        request(base / id / "json")(handler)

      /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#get-the-history-of-an-image */
      def history = complete[List[Record]](base / id / "history")

      // todo insert stream
      def insert(url: String, path: String) =
        stream[Unit](base.POST / id  / "insert"
                     <<? Map("url" -> url, "path" -> path))

      def push = Push()

      def tag = Tag()

      def delete = Delete()
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#search-images */
    case class Search(
      private val _term: Option[String] = None)
      extends Docker.Completion[List[SearchResult]] {
      def term(t: String) = copy(_term = Some(t))
      def apply[T](handler: Docker.Handler[T]) =
        request(base / "search" <<? _term.map(("term" -> _)))(handler)
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.16/#build-an-image-from-dockerfile-via-stdin */
    case class Build(
      path: File,
      private val _tag: Option[String]      = None,
      private val _q: Option[Boolean]       = None,
      private val _nocache: Option[Boolean] = None,
      private val _rm: Option[Boolean]      = None,
      private val _forcerm: Option[Boolean] = None)
      extends Docker.Stream[tugboat.Build.Output] {
      lazy val tarfile = if (path.isDirectory) {
        Tar(path, TmpFile.create, path.getName, zip = true)
      } else path

      def tag(t: String) = copy(_tag = Some(t))
      def verbose(v: Boolean) = copy(_q = Some(!v))
      def nocache(n: Boolean) = copy(_nocache = Some(n))
      def rm(r: Boolean) = copy(_rm = Some(r))
      def forceRm(r: Boolean) = copy(_forcerm = Some(r))
      def apply[T](handler: Docker.Handler[T]) =
        request((host.POST / "build" <:< Map(
                "Content-Type" -> "application/tar",
                "Content-Encoding" -> "gzip") ++ authConfig.map(
                  ("X-Registry-Auth" -> _.headerValue)
                 ) <<? query)
                <<< tarfile)(handler)

      def query = (
        Map.empty[String, String]
          ++ _tag.map(("t" -> _))
          ++ _q.map(("q" -> _.toString))
          ++ _nocache.map(("nocache" -> _.toString))
          ++ _rm.map(("rm" -> _.toString))
          ++ _forcerm.map(("forcerm" -> _.toString)))
    }

    /** https://docs.docker.com/reference/api/docker_remote_api_v1.15/#list-images */
    def list = Images()

    // the api calls this create by the default client calls this pull
    // pull seems more `intention revealing` so let's use that
    def pull(image: String) = Pull(image)
    // but to avoid confustion let's alias it for those reading from the docs
    def create = pull _
    def get(id: String) = Image(id)
    def search = Search()
    /** if path is a directory, it will be bundled into a gzipped tar. otherwise we assume a tar file */
    def build(path: File) = Build(path)
  }
}
