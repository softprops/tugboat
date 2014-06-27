package tugboat

import java.io.{
  BufferedInputStream, BufferedOutputStream, File,
  FileInputStream, FileOutputStream, InputStream }
import java.util.zip.{ Deflater, GZIPOutputStream }
import org.kamranzafar.jtar.{ TarEntry, TarOutputStream }

object Tar {
  /**
   *  Creates a gzipped tar file from a target directory
   *  @param inDir directory to create a tar archive from
   *  @param outDir directory to create tar file in
   *  @param name the name of the tar archive ( excluding the file extention ) */
  def apply(inDir: File, outDir: File, name: String) = {
    val buffer = Array.fill(1024 * 1024)(0: Byte)
    val tarStream = new TarOutputStream(
      new BufferedOutputStream(
        new GZIPOutputStream(
          new FileOutputStream(new File(outDir, s"${name}.tgz"))) {
            `def`.setLevel(Deflater.BEST_COMPRESSION)
          }
      )
    )

    def include(src: File, dst: String) = {
      tarStream.putNextEntry(new TarEntry(src, dst) {
        setIds(0, 0)
        setUserName("")
        setGroupName("")
        if (src.canExecute) {
          getHeader.mode = Integer.parseInt("0755", 8)
        }
      })
      tarStream
    }

    def loop(dir: File): Unit =
      dir.listFiles.foreach { file =>
        include(file, name + "/" + relativeTo(inDir, file).getOrElse(file).toString)
        if (file.isDirectory) loop(file) else {
          def copy(input: InputStream): Unit = input.read(buffer) match {
            case len if len > -1 =>
              tarStream.write(buffer, 0, len)
              copy(input)
            case _ => input.close()
          }
          copy(new BufferedInputStream(new FileInputStream(file)))
        }
      }

      loop(inDir)
      tarStream.close()
  }

  // thanks sbt ( mark ) ...

  def relativeTo(base: File, file: File): Option[File] =
    relativize(base, file).map(new File(_))

  def relativize(base: File, file: File): Option[String] = {
    def baseFileString(baseFile: File): Option[String] = {
      if (baseFile.isDirectory) {
        val cp = baseFile.getAbsolutePath
        assert(cp.length > 0)
        val normalized = if (cp.charAt(cp.length - 1) == File.separatorChar) cp else cp + File.separatorChar
        Some(normalized)
      } else None
    }
    val pathString = file.getAbsolutePath
    baseFileString(base).filter(pathString.startsWith).map { baseString =>
      pathString.substring(baseString.length)
    }
  }
}
