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

Tugboat provides interfaces for interacting with docker over http returning Scala [Futures](http://www.scala-lang.org/api/current/index.html#scala.concurrent.Future) containing docker responses. This requires docker is reachable via tcp. See the docker [documentation](https://docs.docker.com/articles/basics/#bind-docker-to-another-hostport-or-a-unix-socket) on how to set this up. In some environments like [boot2docker](https://github.com/boot2docker/boot2docker), this is the default.

Tugboat will work out of the box with various conventional env vars set.

name        | value
------------|--------------------------------------------------------------------------------------
DOCKER_HOST | the tcp host where the docker daemon is bound to
CERT_PATH   | the base path of were docker tls key.pem, cert.pem, and ca.pem files will be resolved
TLS_VERIFY  | any non empty string is considered truthy

_note_: the unix domain socket interface is not yet supported but is planned for the future

You can discover this for yourself by typing the following in your terminal

```bash
$ echo $DOCKER_HOST
```

Specific API interfaces like `build`, `events`, `logs`, `push` and `pull`, responses are streamed.

Below are some examples of manning your local dock.

```scala
import scala.concurrent.ExecutionContext.Implicits.global

// create a docker tugboat client
// you can also provide a host url as an argument
val docker = tugboat.Docker()

// inspect your station
// $docker info
docker.info().foreach(println)

// print the make and model of docker harbor
// $ docker version
docker.version().foreach(println)

// search the sea of docker images for something that looks like a ship
// $ docker search ship
docker.images.search.term("ship")().map(_.map(_.name)).foreach(println)

// list the images docked at your station
// $ docker images
docker.images.list().map(_.map(_.id)).foreach(println)

// be your own shipping port

// keep an close eye on activity in your harbor
import tugboat.Event
val (stopper, completeFuture) = docker.events.stream {
  case Event.Record(status, id, from, time) =>
     println(s"container $id: $status")
}

// ...fire up, start, stop, and remove some containers
// to terminate the stream, call stop() on the stopper returned by subscribing
// to the stream
stopper.stop()


// usher a ship out to sea
// $ docker build -t ssScala path/to/dir/Dockerfile/is/in
import tugboat.Build 
docker.images.build(new java.io.File("path/to/dir/Dockerfile/is/in")).tag("ssScala").stream {
  case Build.Progress(prog)   => println(prog)
  case Build.Status(status)   => println(status)
  case Build.Error(err, _, _) => println(err)
}

// usher foreign ships into harbor
// $ docker pull captain/ship
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


// verify with the admiral that your captain credentials are still respectable
// $ docker login -u username -p password -e email
import tugboat.AuthConfig
val auth = AuthConfig(user, password, email)
docker.auth(auth)().foreach(println)

// announce your captainship when issuing orders to the crew
// $ docker pull internalregistry.com/captain/ship
docker.as(auth).images.pull("captain/ship").registry("internalregistry.com").stream {
  case Pull.Status(msg) => println(msg)
  case Pull.Progress(msg, _, details) =>
    println(msg)
    details.foreach { dets =>
      println(dets.bar)
    }
  case Pull.Error(msg, _) =>  println(msg)
}

// fashion a new boat from a dependable stack of material and start the engines
// $ docker run -p 80:80 captain/ship
(for {
  container <- docker.containers.create("captain/ship")()
  run       <- docker.containers.get(container.id).start.bind(
               tugboat.Port.Tcp(80), tugboat.PortBinding.local(80)
            )()
} yield container.id).foreach(println)

// produce a roster of ships out to sea
// $ docker ps
docker.containers.list().map(_.map(_.id)).foreach(println)

// anchor to a live boat
val ship = docker.containers.get(id)

// inspect the boat
// $ docker inspect ship
ship().foreach(println)

// fetch the the captains logs
// $ docker logs ship
ship.logs.follow(true).stdout(true).stderr(true).stream(println)

// stop the boat after 5 seconds
// $ docker stop ship
import scala.concurrent.duration._
ship.stop(5.seconds)().foreach(println)

// restart the boat in 5 second
// $ docker restart ship
ship.restart(5.seconds)().foreach(println)

// retire the ship
// $ docker rm ship
ship.destroy.force(true)().foreach(println)
```

Doug Tangren (softprops) 2014
