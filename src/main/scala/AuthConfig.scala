package tugboat

import org.json4s.native.JsonMethods.{ compact, render }
import org.json4s.JsonDSL._

case class AuthConfig(
  user: String,
  password: String,
  email: String,
  server: String = "https://index.docker.io/v1/") {
  lazy val headerValue = Base64.encode(compact(render(
    ("username" -> user) ~
    ("password" -> password) ~
    ("email" -> email) ~
    ("serveraddress" -> server))))
}
