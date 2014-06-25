package tugboat

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req, url, :/ }
import scala.concurrent.{ ExecutionContext, Future }

object Client {
  type Handler[T] = AsyncHandler[T]
  trait Completion[T] {
    def asRep: Response => T
    def apply(): Future[T] =
      apply(asRep)
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f))
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
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
      def asRep = identity(_)
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}


case class Client(
  hostStr: String,
  private val http: Http = Http)
  (implicit ec: ExecutionContext)
  extends Requests(hostStr, http)
