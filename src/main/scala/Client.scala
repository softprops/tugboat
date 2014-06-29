package tugboat

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req, stream, url, :/ }
import dispatch.stream.StringsByLine
import scala.concurrent.{ ExecutionContext, Future }

object Client {
  private[tugboat] val UserAgent = s"tugboat/${BuildInfo.version}"
  private[tugboat] val DefaultHeaders = Map("User-Agent" -> UserAgent)
  type Handler[T] = AsyncHandler[T]
  /** mixin used for uniform request handler completion */
  trait Completer {
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
  }
  /** extension of completer providing a default rep of response */
  abstract class Completion[T: Rep] extends Completer {
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f))
  }
  /** extension of completer providing a default rep of the items within
   *  a streamed response */
  abstract class Stream[T: StreamRep] extends Completer {
    type Handler = T => Unit
    protected def streamer: Handler => Client.Handler[Unit] = { f =>
      new StringsByLine[Unit] {
        def onStringBy(str: String) {
          f(implicitly[StreamRep[T]].map(str))
        }
        def onCompleted = ()
      }
    }
    def stream(f: Handler): Future[Unit] =
      apply(streamer(f))
  }
}

abstract class Requests(
   hostStr: String, http: Http)
  (implicit ec: ExecutionContext)
  extends Methods {

  def host = url(hostStr)

  def request[T]
    (req: Req)
    (handler: Client.Handler[T]): Future[T] =
    http(req <:< Client.DefaultHeaders > handler)

  def complete(req: Req): Client.Completion[Response] =
    new Client.Completion[Response] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}

case class Client(
  hostStr: String,
  private val http: Http = new Http)
  (implicit ec: ExecutionContext)
  extends Requests(hostStr, http) {
  /** releases the underlying http client's resources.
   *  after close() is invoked, all behavior for this
   *  client is undefined */
  def close() = http.shutdown()
}
