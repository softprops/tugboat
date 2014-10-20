package tugboat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

case class Version(
  apiVersion: String,
  version: String,
  gitCommit: String,
  goVersion: String)

case class Info(
  containers: Int,
  images: Int,
  driver: String,
  executionDriver: String,
  kernelVersion: String,
  debug: Int, nFd: Int,
  nGoroutines: Int, nEventsListener: Int,
  initPath: String, indexServerAddr: String,
  memoryLimit: Int, swapLimit: Int, ipv4Forwarding: Int)

case class PortDesc(ip: String, priv: Int, pub: Int, typ: String)

case class Container(
  id: String, image: String, cmd: String, created: Long, status: String,
  ports: Seq[PortDesc], names: Seq[String],
  sizeRw: Option[Long] = None, sizeRootFs: Option[Long] = None)

object Create {
  case class Response(id: String, warnings: Seq[String])
}

case class ContainerConfig(
  image: String,
  attachStdin: Boolean      = false,
  attachStdout: Boolean     = false,
  attachStderr: Boolean     = false,
  cmd: Seq[String]          = Seq.empty,
  cpuShares: Int            = 0,
  cpuSet: String            = "",
  domainName: String        = "",
  entryPoint: Seq[String]   = Seq.empty,
  env: Map[String, String]  = Map.empty,
  exposedPorts: Seq[String] = Seq.empty,
  hostname: String          = "",
  memory: Long              = 0,
  memorySwap: Long          = 0,
  networkDisabled: Boolean  = false,
//  onBuild: ???
  openStdin: Boolean        = false,
  stdinOnce: Boolean        = false,
  user: String              = "",
  tty: Boolean              = false,
  volumes: Seq[String]      = Seq.empty,
  workingDir: String        = ""
)

case class ContainerState(
  running: Boolean,
  paused: Boolean,
  pid: Int,
  exitCode: Int,
  started: String,
  finished: String)

// https://github.com/dotcloud/docker/blob/master/nat/nat.go#L19
case class PortBinding(hostIp: String, hostPort: Int)
// docker -p  format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort
object PortBinding {
  def local(port: Int) = PortBinding("0.0.0.0", port)
}

object VolumeBinding {
  val Spec = """(.*):(.*)""".r
  val Restricted = """(.*):(.*):(.*)""".r
  trait Mode {
    def value: String = getClass.getSimpleName.stripSuffix("$")
  }
  case object RO extends Mode
  case object RW extends Mode
  def parse(str: String) = str match {
    case Spec(host, container) =>
      VolumeBinding(host, container)
    case Restricted(host, container, mode) =>
      VolumeBinding(host, container, mode match {
        case "ro" => Some(RO)
        case "rw" => Some(RW)
        case _ => None
      })
  }
}

case class VolumeBinding(
  hostPath: String, containerPath: String,
  mode: Option[VolumeBinding.Mode] = None) {
  lazy val spec = s"$hostPath:$containerPath${mode.map(m => ":" + m.value).getOrElse("")}"
}

object VolumeFromBinding {
  val Restricted = """(.*):(.*)""".r
  def parse(str: String) = str match {
    case Restricted(container, mode) => VolumeFromBinding(container, mode match {
      case "ro" => Some(VolumeBinding.RO)
      case "rw" => Some(VolumeBinding.RW)
      case _ => None
    })
    case container => VolumeFromBinding(container)
  }
}

case class VolumeFromBinding(
  container: String, mode: Option[VolumeBinding.Mode] = None) {
  lazy val spec = s"$container${mode.map(m => ":" + m.value).getOrElse("")}"
}

case class NetworkSettings(
  bridge: String, gateway: String, ipAddr: String, ipPrefixLen: Int, ports: Map[Port, List[PortBinding]])

// https://github.com/dotcloud/docker/blob/v1.0.1/runconfig/hostconfig.go#L22
case class HostConfig(
  binds: Seq[VolumeBinding]             = Seq.empty,
  containerIdFile: String               = "",
  lxcConf: Seq[String]                  = Seq.empty,
  privileged: Boolean                   = false,
  ports: Map[Port, List[PortBinding]]   = Map.empty,
  links: Seq[String]                    = Seq.empty,
  publishAllPorts: Boolean              = false,
  dns: Seq[String]                      = Seq.empty,
  dnsSearch: Seq[String]                = Seq.empty,
  volumesFrom: Seq[VolumeFromBinding]   = Seq.empty,
  networkMode: NetworkMode              = NetworkMode.Bridge,
  capAdd: Seq[String]                   = Seq.empty,
  capDrop: Seq[String]                  = Seq.empty)

case class ContainerDetails(
  id: String, name: String, created: String, path: String, hostnamePath: String, hostsPath: String,
  args: Seq[String], config: ContainerConfig, state: ContainerState, image: String,
  networkSettings: NetworkSettings, resolvConfPath: String, volumes: Map[String, String],
  volumesRW: Map[String, Boolean], hostConfig: HostConfig)

case class Record(id: String, created: Long, createdBy: String, size: Long, tags: Seq[String])

case class Top(titles: Seq[String], procs: Seq[Seq[String]])

case class Change(path: String, kind: Int)

case class Status(code: Int)

case class Image(
  id: String, created: Long, size: Long, virtualSize: Long,
  repoTags: List[String] = Nil, parent: Option[String] = None)

case class ImageDetails(
  id: String, created: String, container: String, size: Long, parent: Option[String], config: ContainerConfig)

case class SearchResult(
  name: String, description: String, trusted: Boolean, official: Boolean, stars: Int)

case class ImageDeletionStatus(status: String, id: String)

/** type class for default representations */
sealed trait Rep[T] {
  def map: Response => T
}

object Rep {
  implicit val Identity: Rep[Response] = new Rep[Response] {
    def map = identity(_)
  }

  implicit val Nada: Rep[Unit] = new Rep[Unit] {
    def map = _ => ()
  }

  implicit val Versions: Rep[Version] = new Rep[Version] {
    def map = { r =>
      (for {
        JObject(version) <- as.json4s.Json(r)
        ("ApiVersion", JString(api)) <- version
        ("Version", JString(ver))    <- version
        ("GitCommit", JString(git))  <- version
        ("GoVersion", JString(go))   <- version
      } yield Version(api, ver, git, go)).head
    }
  }

  implicit val Infos: Rep[Info] = new Rep[Info] {
    def map = { r =>
      (for {
        JObject(info) <- as.json4s.Json(r)
        ("Containers", JInt(cont)) <- info
        ("Images", JInt(images))               <- info
        ("Driver", JString(driver))            <- info
        ("ExecutionDriver", JString(exDriver)) <- info
        ("KernelVersion", JString(kVersion))   <- info
        ("Debug", JInt(debug))                 <- info
        ("NFd", JInt(nfd))                     <- info
        ("NGoroutines", JInt(ngor)) <- info
        ("NEventsListener", JInt(nevl)) <- info
        ("InitPath", JString(initpath)) <- info
        ("IndexServerAddress", JString(indexSvrAddr)) <- info
        ("MemoryLimit", JInt(memLim)) <- info
        ("SwapLimit", JInt(swapLim)) <- info
        ("IPv4Forwarding", JInt(ipv4)) <- info
      } yield Info(
        cont.toInt, images.toInt, driver, exDriver, kVersion,
        debug.toInt, nfd.toInt, ngor.toInt, nevl.toInt, initpath,
        indexSvrAddr, memLim.toInt, swapLim.toInt, ipv4.toInt)).head
    }
  }

  implicit val ListOfContainers: Rep[List[Container]] = new Rep[List[Container]] {
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
      } yield PortDesc(ip, priv.toInt, pub.toInt, typ),
      for {
        ("Names", JArray(names)) <- cont
        JString(name)            <- names
      } yield name,
      (for ( ("SizeRw", JInt(size)) <- cont)
       yield size.toLong).headOption,
      (for ( ("SizeRootFs", JInt(size)) <- cont)
       yield size.toLong).headOption
    )))
  }

  implicit val CreateResponse: Rep[Create.Response] = new Rep[Create.Response] {
    def map = { r => (for {
      JObject(resp)       <- as.json4s.Json(r)
      ("Id", JString(id)) <- resp
    } yield Create.Response(id, for {
      ("Warnings", JArray(warns)) <- resp
      JString(warn)               <- warns
    } yield warn)).head }
  }

  implicit object ContainerDetail extends Rep[Option[ContainerDetails]] {
    private[this] val KeyVal = """(.+)=(.+)""".r

    def map = { r => (for {
      JObject(cont)                                <- as.json4s.Json(r)
      ("Id", JString(id))                          <- cont
      ("Name", JString(name))                      <- cont
      ("Created", JString(created))                <- cont
      ("Path", JString(path))                      <- cont
      ("Image", JString(img))                      <- cont
      ("HostsPath", JString(hostsPath))            <- cont
      ("HostnamePath", JString(hostnamePath))      <- cont
      ("ResolvConfPath", JString(resolveConfPath)) <- cont
    } yield ContainerDetails(
      id, name, created, path, hostsPath, hostnamePath, for {
        ("Args", JArray(args)) <- cont
        JString(arg)           <- args
      } yield arg, (for {
        ("Config", JObject(cfg)) <- cont
      } yield containerConfig(cfg)).head,
      containerState(cont),
      img,
      containerNetworkSettings(cont),
      resolveConfPath, (for {
        ("Volumes", JObject(volumes)) <- cont
        (from, JString(to))           <- volumes
      } yield (from, to)).toMap, (for {
        ("VolumesRW", JObject(volumes)) <- cont
        (vol, JBool(rw)) <- volumes
      } yield (vol, rw)).toMap,
      containerHostConfig(cont))).headOption
    }

    private def strs(v: JValue) = for {
      JArray(strs) <- v
      JString(str) <- strs
    } yield str

    private def containerHostConfig(cont: List[JField]) =
      (for {
        ("HostConfig", JObject(config))         <- cont
        ("Binds", binds)                        <- config
        ("ContainerIDFile", JString(idFile))    <- config
        ("Dns", dns)                            <- config
        ("DnsSearch", dnsSearch)                <- config
        ("Links", links)                        <- config
        ("LxcConf", lxcConf)                    <- config
        ("PortBindings", JObject(portBindings)) <- config
        ("Privileged", JBool(priv))             <- config
        ("PublishAllPorts", JBool(pubAllPorts)) <- config
        ("VolumesFrom", volumesFrom)            <- config
      } yield HostConfig(
        strs(binds).map(VolumeBinding.parse(_)),
        idFile,
        strs(lxcConf),
        priv,
        (for {
          (Port(port), bindings) <- portBindings        
        } yield (port, (for {
          JArray(xs)                      <- bindings
          JObject(binding)                <- xs
          ("HostIp", JString(hostIp))     <- binding
          ("HostPort", JString(hostPort)) <- binding
        } yield PortBinding(hostIp, hostPort.toInt)))).toMap,
        strs(links),
        pubAllPorts,
        strs(dns),
        strs(dnsSearch),
        strs(volumesFrom).map(VolumeFromBinding.parse(_)), (for {
          ("NetworkMode", JString(NetworkMode(netMode))) <- config
        } yield netMode).headOption.getOrElse(NetworkMode.Bridge),
        for {
          ("CapAdd", adds) <- config
          str <- strs(adds)
        } yield str, for {
          ("CapDrop", drops) <- config
          str <- strs(drops)
        } yield str)).head

    private def containerNetworkSettings(cont: List[JField]) =
      (for {
        ("NetworkSettings", JObject(settings)) <- cont
        ("Bridge", JString(bridge))            <- settings
        ("Gateway", JString(gateway))          <- settings
        ("IPAddress", JString(ip))             <- settings
        ("IPPrefixLen", JInt(prefLen))         <- settings
      } yield NetworkSettings(bridge, gateway, ip, prefLen.toInt, (for {
        ("Ports", JObject(ports)) <- settings
        (Port(port), mappings)          <- ports
      } yield {
        (port, (for {
          JArray(confs)                   <- mappings
          JObject(conf)                   <- confs
          ("HostIp", JString(hostIp))     <- conf
          ("HostPort", JString(hostPort)) <- conf
        } yield PortBinding(hostIp, hostPort.toInt)))
      }).toMap)).head

    private def containerState(cont: List[JField]) =
      (for {
        ("State", JObject(state))       <- cont
        ("Paused", JBool(pause))        <- state
        ("Running", JBool(run))         <- state
        ("Pid", JInt(pid))              <- state
        ("ExitCode", JInt(exit))        <- state
        ("StartedAt", JString(started)) <- state
        ("FinishedAt", JString(fin))    <- state
      } yield ContainerState(
        run, pause, pid.toInt, exit.toInt, started, fin)).head

    def containerConfig(cfg: List[JField]) =
      (for {
        ("Image", JString(img))            <- cfg
        ("AttachStderr", JBool(atStdErr))  <- cfg
        ("AttachStdin", JBool(atStdIn))    <- cfg
        ("AttachStdout", JBool(atStdO))    <- cfg
        ("Cmd", JArray(cmd))               <- cfg
        ("CpuShares", JInt(cpuShares))     <- cfg
        ("Cpuset", JString(cpuSet))        <- cfg
        ("Domainname", JString(domain))    <- cfg
        ("Env", JArray(env))               <- cfg
        ("ExposedPorts", JObject(ports))   <- cfg
      } yield ContainerConfig(
        img, atStdErr, atStdIn, atStdO, for {
          JString(arg) <- cmd
        } yield arg, cpuShares.toInt, cpuSet,
        domain, (for {
          ("Entrypoint", JArray(ep)) <- cfg
          JString(e)                 <- ep
        } yield e), (for {
          JString(KeyVal(k, v)) <- env
        } yield (k, v)).toMap)).head
  }

  implicit val ListOfRecord: Rep[List[Record]] = new Rep[List[Record]] {
    def map = (as.json4s.Json andThen (for {
      JArray(events)              <- _
      JObject(event)              <- events
      ("Id", JString(id))         <- event
      ("Created", JInt(created))  <- event
      ("CreatedBy", JString(by))  <- event
      ("Size", JInt(size))        <- event
    } yield Record(id, created.toLong, by, size.toLong, for {
      ("Tags", JArray(tags)) <- event
      JString(tag)           <- tags
    } yield tag)))
  }

  implicit val Tops: Rep[Top] = new Rep[Top] {
    def map = { r => (for {
     JObject(top)                 <- as.json4s.Json(r)
     ("Titles", JArray(titles))   <- top
     ("Processes", JArray(procs)) <- top
    } yield Top(
      for ( JString(title) <- titles ) yield title,
      for ( JArray(infos) <- procs ) yield for {
        JString(info) <- infos
      } yield info)).head
    }
  }

  implicit val ListOfChanges: Rep[List[Change]] = new Rep[List[Change]] {
    def map = (as.json4s.Json andThen (for {
      JArray(changes) <- _
      JObject(change) <- changes
      ("Path", JString(path)) <- change
      ("Kind", JInt(kind)) <- change
    } yield Change(path, kind.toInt)))
  }

  implicit val StatusCode: Rep[Status] = new Rep[Status] {
    def map = { r => (for {
      JObject(status)            <- as.json4s.Json(r)
      ("StatusCode", JInt(code)) <- status
    } yield Status(code.toInt)).head
   }
  }

  implicit val ListOfImages: Rep[List[Image]] = new Rep[List[Image]] {
    def map = (as.json4s.Json andThen (for {
        JArray(imgs)                 <- _
        JObject(img)                 <- imgs
        ("Id", JString(id))          <- img
        ("Created", JInt(created))   <- img
        ("Size", JInt(size))         <- img
        ("VirtualSize", JInt(vsize)) <- img
      } yield tugboat.Image(
        id, created.toLong, size.toLong, vsize.toLong, for {
          ("RepoTags", JArray(tags)) <- img
          JString(tag) <- tags
        } yield tag, (for {
          ("ParentId", JString(parent)) <- img
        } yield parent).headOption)))
  }

  implicit val ListOfSearchResults: Rep[List[SearchResult]] = new Rep[List[SearchResult]] {
    def map = (as.json4s.Json andThen (for {
        JArray(results)                <- _
        JObject(res)                   <- results
        ("name", JString(name))        <- res
        ("description", JString(desc)) <- res
        ("is_trusted", JBool(trust))   <- res
        ("is_official", JBool(offic))  <- res
        ("star_count", JInt(stars))    <- res
      } yield SearchResult(
        name, desc, trust, offic, stars.toInt)))
  }

  implicit val ImageDetail: Rep[Option[ImageDetails]] = new Rep[Option[ImageDetails]] {
    def map = { r =>
      (for {
        JObject(img)                      <- as.json4s.Json(r)
        ("Id", JString(id))               <- img
        ("Created", JString(created))     <- img
        ("Container", JString(container)) <- img
        ("Size", JInt(size))              <- img
      } yield ImageDetails(
        id, created, container, size.toLong,
        (for {
          ("Parent", JString(parent)) <- img
        } yield parent).headOption,
        (for {
          ("ContainerConfig", JObject(cfg)) <- img
        } yield ContainerDetail.containerConfig(cfg)).head)
      ).headOption
    }
  }

  implicit val ImageDeletions: Rep[List[ImageDeletionStatus]] =
    new Rep[List[ImageDeletionStatus]] {
      def map = (as.json4s.Json andThen (for {
        JArray(statuses)      <- _
        JObject(status)       <- statuses
        (stat, JString(id))   <- status
      } yield ImageDeletionStatus(stat, id)))
    }
}
