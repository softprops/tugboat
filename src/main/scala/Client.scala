package tugboat

import com.ning.http.client.{ AsyncHandler, HttpResponseStatus, Response }
import dispatch.{ FunctionHandler, Http, Req, StatusCode, url, :/ }
import dispatch.stream.StringsByLine
import java.net.URI
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.Exception.allCatch

object Client {
  case class Error(code: Int, message: String) extends RuntimeException(message)

  type Handler[T] = AsyncHandler[T]

  private[tugboat] trait StreamErrorHandler[T] extends AsyncHandler[T] { self: AsyncHandler[T] =>
    abstract override def onStatusReceived(status: HttpResponseStatus) =
      if (status.getStatusCode / 100 == 2)
        super.onStatusReceived(status)
      else
        throw Error(status.getStatusCode, "")
  }

  /** mixin used for uniform request handler completion */
  trait Completer {
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
  }

  /** extension of completer providing a default rep of response */
  abstract class Completion[T: Rep](implicit ec: ExecutionContext) extends Completer {
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

  /** extension of completer providing a default rep of the items within
   *  a streamed response */
  abstract class Stream[T: StreamRep] extends Completer {
    type Handler = T => Unit
    /** @return a future of unit apply a function, f, to each chunk of the streamed response */
    def stream(f: Handler): Future[Unit] =
      apply(streamer(f))

    /** @return a function that takes a function to apply to each chunk of a response
     *          and produces a handler for the request's response */
    protected def streamer: Handler => Client.Handler[Unit] = { f =>
      new StringsByLine[Unit] with StreamErrorHandler[Unit] {
        def onStringBy(str: String) {
          f(implicitly[StreamRep[T]].map(str))
        }
        def onCompleted = ()
      }
    }
  }

  private[tugboat] val UserAgent = s"tugboat/${BuildInfo.version}"
  private[tugboat] val DefaultHeaders = Map("User-Agent" -> UserAgent)
  private[tugboat] val DefaultHost =
    Option(System.getenv("DOCKER_HOST")) // boot2dockerism
      .flatMap( str => allCatch.opt(new URI(str))).map { dockerHost =>
        s"http://${dockerHost.getHost}:${dockerHost.getPort}"
      }.getOrElse("http://localhost:2375")
}

abstract class Requests(
  hostStr: String, http: Http,
  protected val authConfig: Option[AuthConfig])
 (protected implicit val ec: ExecutionContext)
  extends Methods {

  def host = url(hostStr)

  def request[T]
    (req: Req)
    (handler: Client.Handler[T]): Future[T] =
    http(req <:< Client.DefaultHeaders > handler)

  def stream[A: StreamRep](req: Req): Client.Stream[A] =
    new Client.Stream[A] {
      def apply[T](handler: Client.Handler[T]): Future[T] =
        request(req)(handler)
    }

  def complete[A: Rep](req: Req): Client.Completion[A] =
    new Client.Completion[A] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}

/** Entry point into docker communication */
case class Client(
  hostStr: String = Client.DefaultHost,
  private val http: Http = new Http,
  private val _auth: Option[AuthConfig] = None)
 (implicit ec: ExecutionContext)
  extends Requests(hostStr, http, _auth) {

  /** Authenticate requests */
  def as(config: AuthConfig) = copy(_auth = Some(config))

  /** releases the underlying http client's resources.
   *  after close() is invoked, all behavior for this
   *  client is undefined */
  def close() = http.shutdown()
}
