package tugboat

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req, stream, url, :/ }
import dispatch.stream.StringsByLine
import scala.concurrent.{ ExecutionContext, Future }

object Client {
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
    def stream(f: T => Unit): Future[Unit] =
      apply(new StringsByLine[Unit] {
        def onStringBy(str: String) {
          f(implicitly[StreamRep[T]].map(str))
        }
        def onCompleted = ()
      })
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
    http(req > handler)

  def complete(req: Req): Client.Completion[Response] =
    new Client.Completion[Response] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}


case class Client(
  hostStr: String,
  private val http: Http = Http)
  (implicit ec: ExecutionContext)
  extends Requests(hostStr, http)
