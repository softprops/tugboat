package tugboat

import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.base64.{ Base64 => Encoder }
import java.nio.charset.Charset

private [tugboat] object Base64 {
  private[this] val utf8 = Charset.forName("utf8")
  def encode(str: String): String =
    Encoder.encode(
      ChannelBuffers.wrappedBuffer(
        str.getBytes("utf8")), false).toString(utf8)
}
