package tugboat

import com.ning.http.client.{ AsyncHandler, AsyncHttpClientConfig, HttpResponseStatus, ProxyServer, Response }
import com.ning.http.client.providers.netty.NettyAsyncHttpProviderConfig
import dispatch.{ FunctionHandler, Http, Req, StatusCode, url, :/ }
import dispatch.stream.{ Strings, StringsByLine }
import java.net.URI
import java.util.concurrent.{ Executors, ThreadFactory }
import jnr.unixsocket.{ UnixSocketAddress, UnixSocketChannel }
import org.jboss.netty.channel.ChannelPipeline
import org.jboss.netty.channel.socket.{ DefaultSocketChannelConfig, SocketChannel }
import org.jboss.netty.channel.socket.nio.{
  DefaultNioSocketChannelConfig, NioSocketChannel, NioClientSocketChannel, NioClientSocketChannelFactory, NioWorkerPool }
import org.jboss.netty.util.HashedWheelTimer
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.Exception.allCatch
import scala.util.control.NoStackTrace
import unisockets.netty.ClientUdsSocketChannelFactory

object Docker {

  case class Error(code: Int, message: String)
    extends RuntimeException(message) with NoStackTrace

  type Handler[T] = AsyncHandler[T]

  private[tugboat] trait StreamErrorHandler[T]
    extends AsyncHandler[T] { self: AsyncHandler[T] =>
    abstract override def onStatusReceived(status: HttpResponseStatus) =
      if (status.getStatusCode / 100 == 2)
        super.onStatusReceived(status)
      else
        throw Error(status.getStatusCode, "")
  }

  /** mixin used for uniform request handler completion */
  trait Completer {
    def apply[T]
      (handler: Docker.Handler[T]): Future[T]
  }

  /** extension of completer providing a default rep of response */
  abstract class Completion[T: Rep]
   (implicit ec: ExecutionContext) extends Completer {
    /** @return a future of the default representation of the response */
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)
    /** @return a future transformed by Response => T*/
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f) {
          override def onCompleted(response: Response) = {
            if (response.getStatusCode / 100 == 2) f(response)
            else throw Error(
              response.getStatusCode,
              if (response.hasResponseBody) response.getResponseBody else "")
          }
        }).recoverWith {
          case ee: java.util.concurrent.ExecutionException =>
            Future.failed(ee.getCause)
        }
  }

  object Stream {
    /** A interface returned with streamed response to
     *  programatically stop processing mid-stream */
    trait Stopper {
      def stop(): Unit
    }

    def lines[T: StreamRep]: (T => Unit) => Docker.Handler[Unit] with Stream.Stopper = { f =>
      new StringsByLine[Unit] with StreamErrorHandler[Unit]
        with Stream.Stopper {
        def onStringBy(str: String) {
          f(implicitly[StreamRep[T]].map(str))
        }
        def onCompleted = ()
      }
    }

    def chunk[T: StreamRep]: (T => Unit) => Docker.Handler[Unit] with Stream.Stopper = { f =>
      new Strings[Unit] with Docker.StreamErrorHandler[Unit]
        with Docker.Stream.Stopper {
        def onString(str: String) {
          f(implicitly[StreamRep[T]].map(str.trim))
        }
        def onCompleted = ()
      }
    }
  }

  /** extension of completer providing a default rep of the items within
   *  a streamed response.
   *  todo: add an interface for stoping the stream. */
  abstract class Stream[T: StreamRep] extends Completer {
    type Handler = T => Unit

    /** @return a future of unit apply a function, f, to each chunk of the streamed response */
    def stream(f: Handler): (Stream.Stopper, Future[Unit]) = {
      val stopper = streamer(f)
      (stopper, apply(stopper))
    }

    /** @return a function that takes a function to apply to each chunk of a response
     *          and produces a handler for the request's response */
    protected def streamer: Handler => Docker.Handler[Unit] with Stream.Stopper = Stream.lines
  }

  private def env(name: String) = Option(System.getenv(s"DOCKER_$name".toUpperCase))
  private[tugboat] val UserAgent = s"tugboat/${BuildInfo.version}"
  private[tugboat] val DefaultHeaders = Map("User-Agent" -> UserAgent)
  private[tugboat] val DefaultHost = (for {
    host  <- env("HOST")
    uri <- allCatch.opt(new URI(host))
  } yield s"${if(2376 == uri.getPort) "https" else "http"}://${uri.getHost}:${uri.getPort}").getOrElse(
    "unix:///var/run/docker.sock"
  )

  private[tugboat] def host(hostStr: String): Req =
    if (hostStr.startsWith("unix://")) {
      Req(identity).setVirtualHost(hostStr).setProxyServer(new ProxyServer(hostStr, 80))
    } else url(hostStr)

  /** the default Http instance may be tls enabled under certain env conditions
   *  defined here https://docs.docker.com/articles/https/#secure-by-default */
  private[tugboat] def defaultHttp(host: String): Http = {
    val certs  = env("CERT_PATH")
    val verify = env("TLS_VERIFY").filter(_.nonEmpty).isDefined
    val http =
      if (host.startsWith("unix://")) new Http().configure { builder =>
        val config = builder.build()
        def shutdown() {
          sockets.releaseExternalResources()
        }
        lazy val threads = new ThreadFactory {
          def newThread(runnable: Runnable) = {
            new Thread(runnable) {
              setDaemon(true)
              override def interrupt() {
                shutdown()
                super.interrupt()
              }
            }
          }
        }
        lazy val sockets = new ClientUdsSocketChannelFactory(
          Executors.newCachedThreadPool(threads),
          Executors.newCachedThreadPool(threads))
        val updatedProvider = config.getAsyncHttpProviderConfig() match {
          case netty: NettyAsyncHttpProviderConfig =>
            netty.addProperty(
              NettyAsyncHttpProviderConfig.SOCKET_CHANNEL_FACTORY,
              sockets
            )
          case dunno =>
            // user has provided an async client non using a netty provider
            dunno
        }
        new AsyncHttpClientConfig.Builder(config).setAsyncHttpClientProviderConfig(updatedProvider)
      }
      else new Http
    certs match {
      case Some(path) =>
        def pem(name: String) = s"$path/$name.pem"
        http.configure(TLS(pem("key"), pem("cert"), Some(pem("ca")).filter(_ => verify)).certify)
      case _ =>
        http
    }
  }
}

abstract class Requests(
  val host: Req, http: Http,
  protected val authConfig: Option[AuthConfig])
 (protected implicit val ec: ExecutionContext)
  extends Methods {

  protected def client: Http = http

  def request[T]
   (req: Req)
   (handler: Docker.Handler[T]): Future[T] =
    http(req <:< Docker.DefaultHeaders > handler)

  def stream[A: StreamRep](req: Req): Docker.Stream[A] =
    new Docker.Stream[A] {
      def apply[T](handler: Docker.Handler[T]): Future[T] =
        request(req)(handler)
    }

  def complete[A: Rep](req: Req): Docker.Completion[A] =
    new Docker.Completion[A] {
      override def apply[T](handler: Docker.Handler[T]) =
        request(req)(handler)
    }
}

/** Entry point into docker communication */
case class Docker(
  hostStr: String                       = Docker.DefaultHost,
  private val http: Option[Http]        = None,
  private val _auth: Option[AuthConfig] = None)
 (implicit ec: ExecutionContext)
  extends Requests(
    Docker.host(hostStr), http.getOrElse(Docker.defaultHttp(hostStr)), _auth) {

  /** see also https://docs.docker.com/articles/https/ */
  def secure(tls: TLS) = copy(
    hostStr = hostStr.replaceFirst("http", "https"),
    http = Some(client.configure(tls.certify)))

  /** Authenticate requests */
  def as(config: AuthConfig) = copy(_auth = Some(config))

  /** releases the underlying http client's resources.
   *  after close() is invoked, all behavior for this
   *  client is undefined */
  def close() = client.shutdown()
}
