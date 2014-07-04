package tugboat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

case class PortDesc(ip: String, priv: Int, pub: Int, typ: String)

case class Container(
  id: String, image: String, cmd: String, created: Long, status: String,
  ports: Seq[PortDesc], names: Seq[String],
  sizeRw: Option[Long] = None, sizeRootFs: Option[Long] = None)

object Create {
  case class Response(id : String, warnings: Seq[String])
}

case class ContainerConfig(
  image: String,
  attachStdin: Boolean      = false,
  attachStdout: Boolean     = false,
  attachStderr: Boolean     = false,
  cmd: Seq[String]          = Seq.empty,
  cpuShares: Int            = 0,
  cpuSet: String           = "",
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
case class NetworkSettings(
  bridge: String, gateway: String, ipAddr: String, ipPrefixLen: Int, ports: Map[Port, List[PortBinding]])

// https://github.com/dotcloud/docker/blob/v1.0.1/runconfig/hostconfig.go#L22
case class HostConfig(
  binds: Seq[String]                    = Seq.empty,
  containerIdFile: String               = "",
  lxcConf: Seq[String]                  = Seq.empty,
  privileged: Boolean                   = false,
  ports: Map[Port, List[PortBinding]]   = Map.empty,
  links: Seq[String]                    = Seq.empty,
  publishAllPorts: Boolean              = false,
  dns: Seq[String]                      = Seq.empty,
  dnsSearch: Seq[String]                = Seq.empty,
  volumesFrom: Seq[String]              = Seq.empty,
  networkMode: NetworkMode              = NetworkMode.Bridge)

case class ContainerDetails(
  id: String, name: String, created: String, path: String, hostnamePath: String, hostsPath: String,
  args: Seq[String], config: ContainerConfig, state: ContainerState, image: String,
  networkSettings: NetworkSettings, resolvConfPath: String, volumes: Map[String, String],
  volumesRW: Map[String, Boolean], hostConfig: HostConfig)

case class Event(id: String, created: Long, createdBy: String, size: Long, tags: Seq[String])

case class Top(titles: Seq[String], procs: Seq[Seq[String]])

case class Change(path: String, kind: Int)

case class Status(code: Int)

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

  implicit object CreateResponse extends Rep[Create.Response] {
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
      } yield arg,
      containerConfig(cont),
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
        strs(binds),
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
        strs(volumesFrom), (for {
          ("NetworkMode", JString(NetworkMode(netMode))) <- config
        } yield netMode).headOption.getOrElse(NetworkMode.Bridge))).head

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

    private def containerConfig(cont: List[JField]) =
      (for {
        ("Config", JObject(cfg))           <- cont
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

  implicit object ListOfEvents extends Rep[List[Event]] {
    def map = (as.json4s.Json andThen (for {
      JArray(events)              <- _
      JObject(event)              <- events
      ("Id", JString(id))         <- event
      ("Created", JInt(created))  <- event
      ("CreatedBy", JString(by))  <- event
      ("Size", JInt(size))        <- event
    } yield Event(id, created.toLong, by, size.toLong, for {
      ("Tags", JArray(tags)) <- event
      JString(tag)           <- tags
    } yield tag)))
  }

  implicit object Tops extends Rep[Top] {
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

  implicit object ListOfChanges extends Rep[List[Change]] {
    def map = (as.json4s.Json andThen (for {
      JArray(changes) <- _
      JObject(change) <- changes
      ("Path", JString(path)) <- change
      ("Kind", JInt(kind)) <- change
    } yield Change(path, kind.toInt)))
  }

  implicit object StatusCode extends Rep[Status] {
    def map = { r => (for {
      JObject(status)            <- as.json4s.Json(r)
      ("StatusCode", JInt(code)) <- status
    } yield Status(code.toInt)).head
   }
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
        ("is_trusted", JBool(trust))   <- res
        ("is_official", JBool(offic))  <- res
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
