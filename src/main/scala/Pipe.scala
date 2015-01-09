package tugboat

import java.io.{ InputStream, PipedInputStream, PipedOutputStream }

class Pipe {
  private[this] val out = new PipedOutputStream()
  private[this] val in  = new PipedInputStream(out)
  def write(bytes: Array[Byte]) {
    out.write(bytes)
  }
  def close() {
    out.close()
    in.close()
  }

  def pipe[T](f: InputStream => T) = f(in)
}
