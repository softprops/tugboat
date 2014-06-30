package tugboat

import org.json4s.native.JsonMethods.{ compact, render }
import org.json4s.JsonDSL._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.base64.Base64

case class AuthConfig(
  _user: String,
  _pass: String,
  _email: String,
  _server: String = "https://index.docker.io/v1/") {
  lazy val headerValue = Base64.encode(
    ChannelBuffers.wrappedBuffer(compact(render(
    ("username" -> _user) ~
    ("password" -> _pass) ~
    ("email" -> _email) ~
    ("serveraddress" -> _server))).getBytes("utf8"))).array()
}
