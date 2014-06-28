# tugboat

> a small boat that maneuvers [Docker](http://www.docker.com/) vessels

[Docker](http://www.docker.com/) is a manager for containment of applications. Tugboat is a small library
that speaks the docker protocol for moving applications and containers in an out of your seaport.

## usage

Tugboat provides interfaces for interacting with docker which return [Futures](http://www.scala-lang.org/api/current/index.html#scala.concurrent.Future).
It's up to your application to deside how to deal with the result of a future.

```scala
import scala.concurrent.ExecutionContext.Implicits.global

// create a docker tugboat client
val tb = tugboat.Client("http://dockerhost:port")

// search the sea of docker images for something that looks like a ship
tb.images.search.term("ship")().map(_.map(_.name)).onComplete(println)

// list the images docked at your station
tb.images.list().map(_.map(_.id)).onComplete(println)

// be your own shipping port
import java.io.File
// tar + gzip a dockerized dir
tugboat.Tar(new File("path/to/dir/with/Dockerfile/in"), new File("."), "app", zip = true)
// usher a ship out to sea
tb.images.build(new File("app.tgz")).tag("ssScala") {
  case BuildOutput.Progress(prog)   => println(prog)
  case BuildOutput.Error(err, _, _) => println(err)
}
```

Doug Tangren (softprops) 2014
