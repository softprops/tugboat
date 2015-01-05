# tugboat

> a small boat that maneuvers [Docker](http://www.docker.com/) vessels

[Docker](http://www.docker.com/) is a manager for the containment of applications. Tugboat is a small library
that speaks the docker protocol for moving applications and containers in and out of your local seaport.

## install

Add the following to your sbt project definition

```scala
resolvers += "softprops-maven" at "http://dl.bintray.com/content/softprops/maven"

libraryDependencies += "me.lessis" %% "tugboat" % "0.1.0"
```

## usage

Tugboat provides interfaces for interacting with a docker daemon over http, returning Scala [Futures](http://www.scala-lang.org/api/current/index.html#scala.concurrent.Future) containing docker responses. This requires docker is resolvable via tcp. See the docker [documentation](https://docs.docker.com/articles/basics/#bind-docker-to-another-hostport-or-a-unix-socket) on how to set this up. In some docker-ready environments like [boot2docker](https://github.com/boot2docker/boot2docker), this is the default.

Tugboat will work out of the box assuming a unix socket `unix:///var/run/docker.sock` but will adapt when various conventional env vars set.

name        | value
------------|--------------------------------------------------------------------------------------
DOCKER_HOST | the tcp host where the docker daemon is bound to
CERT_PATH   | the base path of were docker tls key.pem, cert.pem, and ca.pem files will be resolved
TLS_VERIFY  | any non empty string is considered truthy

You can discover the values for these yourself by typing the following in your terminal

```bash
$ echo $DOCKER_HOST
```

Specific API interfaces like `build`, `events`, `logs`, `push` and `pull`, responses are streamed.

Below are some examples of manning your local dock.

Since we will be interacting with Scala Future's, an implicit `ExecutionContext` is assumed

```scala
import scala.concurrent.ExecutionContext.Implicits.global
```

Create a docker tugboat client. You may optionally supply your docker daemon's host as
a constructor argument

```scala
val docker = tugboat.Docker()
```

Each operation defined on a docker will define an `apply` interface or a `stream` interface depending no the type of 
response the docker daemon serves. Apply interfaces, given no argument, will return a Scala representation of the
resuling json. You may define your'e own handlers for responses as `Response => T` where `T` is the type you wish to return.
The examples below assume the default Scala representations.

### meta

Inspect your station. Identical to the `docker info` command.

```scala
docker.info().foreach(println)
```

Print the make and model of docker harbor. Identical to the `docker version` command.

```scala
docker.version().foreach(println)
```

### images and containers

Search the sea of docker images for something that looks like a ship. 
Identical to the `docker search ship` command.

```scala
docker.images.search
  .term("ship")().foreach {
    _.foreach { img =>
      println(img.name)
    }
  }
```

List the images docked at your station
Identical to the  `docker images` command.

```scala
docker.images.list().foreach {
  _.foreach { image =>
    println(image.id)
  }
}
```

Be your own shipping port

Keep an close eye on activity in your harbor

```scala
import tugboat.Event
val (stopper, completeFuture) = docker.events.stream {
  case Event.Record(status, id, from, time) =>
     println(s"container $id: $status")
}
```

// ...fire up, start, stop, and remove some containers
// to terminate the stream, call stop() on the stopper returned by subscribing
// to the stream
stopper.stop()

Usher a ship out to sea
Identical to the command `docker build -t ssScala path/to/dir/Dockerfile/is/in`

```scala
import tugboat.Build 
docker.images.build(new java.io.File("path/to/dir/Dockerfile/is/in")).tag("ssScala").stream {
  case Build.Progress(prog)   => println(prog)
  case Build.Status(status)   => println(status)
  case Build.Error(err, _, _) => println(err)
}
```

Usher foreign ships into harbor
Identical to the `docker pull captain/ship` command

```
import tugboat.Pull
docker.images.pull("captain/ship").stream {
  case Pull.Status(msg) => println(msg)
  case Pull.Progress(msg, _, details) =>
    println(msg)
    details.foreach { dets =>
      println(dets.bar)
    }
  case Pull.Error(msg, _) =>  println(msg)
}
```


Verify with the admiral that your captain credentials are still respectable
Identical to the `docker login -u username -p password -e email` command

```scala
import tugboat.AuthConfig
val auth = AuthConfig(user, password, email)
docker.auth(auth)().foreach(println)
```

Announce your captainship when issuing orders to the crew
Identical to the `docker pull internalregistry.com/captain/ship` command

```scala
docker.as(auth).images.pull("captain/ship").registry("internalregistry.com").stream {
  case Pull.Status(msg) => println(msg)
  case Pull.Progress(msg, _, details) =>
    println(msg)
    details.foreach { dets =>
      println(dets.bar)
    }
  case Pull.Error(msg, _) =>  println(msg)
}
```

Fashion a new boat from a dependable stack of material and start the engines
Identical to the `docker run -p 80:80 captain/ship` command

```scala
(for {
  container <- docker.containers.create("captain/ship")()
  run       <- docker.containers.get(container.id).start.bind(
               tugboat.Port.Tcp(80), tugboat.PortBinding.local(80)
            )()
} yield container.id).foreach(println)
```

Produce a roster of ships out to sea
Identical to the `docker ps` command

```scala
docker.containers.list().map(_.map(_.id)).foreach(println)
```


Anchor to a live boat

```scala
val ship = docker.containers.get(id)
```

Inspect the boat.
Identical to the `docker inspect ship` command

```scala
ship().foreach(println)
```

Fetch the the captains logs
Identical to the `docker logs ship` command

```scala
ship.logs.follow(true).stdout(true).stderr(true).stream(println)
```

Stop the boat after 5 seconds
Identical to the `docker stop ship` command

```scala
import scala.concurrent.duration._
ship.stop(5.seconds)().foreach(println)
```

Restart the boat in 5 seconds
Identical to the `docker restart ship` command

```scala
ship.restart(5.seconds)().foreach(println)
```

Eetire the ship
Identical to the `docker rm ship` command

```scala
ship.destroy.force(true)().foreach(println)
```

Doug Tangren (softprops) 2014-2015
