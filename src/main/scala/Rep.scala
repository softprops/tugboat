package tugboat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._

case class Port(ip: String, priv: Int, pub: Int, typ: String)

case class Container(
  id: String, image: String, cmd: String, created: Long, status: String,
  ports: Seq[Port], names: Seq[String],
  sizeRw: Option[Long] = None, sizeRootFs: Option[Long] = None)


object Create {
  case class Response(id : String, warnings: Seq[String])
}

case class ContainerConfig(
  image: String,
  cmd: Seq[String]         = Seq.empty,
  hostname: String          = "",
  user: String              = "",
  memory: Long              = 0,
  memorySwap: Long          = 0,
  attachStdin: Boolean      = false,
  attachStdout: Boolean     = true,
  attachStderr: Boolean     = true,
  portSpecs: Option[String] = None, // ???
  tty: Boolean              = false,
  openStdin: Boolean        = false,
  stdinOnce: Boolean        = false,
  env: Map[String, String]  = Map.empty,
  volumes: Seq[String]      = Seq.empty,
  workingDir: String        = "",
  disableNetwork: Boolean   = false,
  exposedPorts: Seq[String] = Seq.empty  
)

case class ContainerState(
  running: Boolean,
  paused: Boolean,
  pid: Int,
  exitCode: Int,
  started: String,
  finished: String)

case class NetworkSettings()

case class HostConfig()

case class ContainerDetails(
  id: String, name: String, created: String, path: String, hostnamePath: String, hostsPath: String,
  args: Seq[String], config: ContainerConfig, state: ContainerState, image: String,
  networkSettings: NetworkSettings, resolvConfPath: String, volumes: Seq[String], hostConfig: HostConfig)

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
      } yield Port(ip, priv.toInt, pub.toInt, typ),
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

  implicit object CreateResponse extends Rep[Option[Create.Response]] {
    def map = { r => (for {
      JObject(resp)       <- as.json4s.Json(r)
      ("Id", JString(id)) <- resp
    } yield Create.Response(id, for {
      ("Warnings", JArray(warns)) <- resp
      JString(warn)               <- warns
    } yield warn)).headOption }
  }

  implicit object ContainerDetail extends Rep[Option[ContainerDetails]] {
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
        ("Image", JString(img)) <- cfg
      } yield ContainerConfig(img)).head, (for {
        ("State", JObject(state))       <- cont
        ("Paused", JBool(pause))        <- state
        ("Running", JBool(run))         <- state
        ("Pid", JInt(pid))              <- state
        ("ExitCode", JInt(exit))        <- state
        ("StartedAt", JString(started)) <- state
        ("FinishedAt", JString(fin))    <- state
      } yield ContainerState(
        run, pause, pid.toInt, exit.toInt, started, fin)).head, img,
      NetworkSettings(), resolveConfPath, Nil, HostConfig())).headOption
    }
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
