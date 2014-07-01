package tugboat

import java.lang.{ Integer => JInt }
import java.io.File
import java.util.Random

private[tugboat] object TmpFile {
  private lazy val random = new Random
  /** creates a new random temporary dir */
  def create = {
    val f = new File(
      System.getProperty("java.io.tmpdir"),
      "tugboat-" + JInt.toHexString(random.nextInt))
    f.mkdir()
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run = if (f.exists) {
        def delete(file: File): Unit =
          file match {
            case dir if (dir.isDirectory) =>
              dir.listFiles.foreach(delete)
              dir.delete()
            case plain =>
              plain.delete()
          }
        delete(f)
      }
    })
    f
  }
}
