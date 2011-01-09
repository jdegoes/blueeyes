import java.io.{File, InputStream, ByteArrayOutputStream, IOException}
import sbt._
import Process._
import java.lang.{ProcessBuilder => JProcessBuilder}

object ExtermalProcess{
  def apply(command: List[String], input: Option[String], directory: Option[String], log: Logger) ={

    val outputStream = new ByteArrayOutputStream
    val errorStream  = new ByteArrayOutputStream

    var builder = new JProcessBuilder(command.toArray : _*)  
    builder = directory.map(v => builder.directory(new File(v))).getOrElse(builder)

    val process = builder run (new ProcessIO(writeTo(input), pumpStream(outputStream, log) _, pumpStream(errorStream, log) _))

    val exitValue = process.exitValue

    if (errorStream.size() != 0) log.error(new String(errorStream.toByteArray))

    val output = new String(outputStream.toByteArray)

    log.info(output)    

    if (exitValue != 0) {
      throw new Exception("Process exitValue=" + process.exitValue)
    }

    output
  }

  private def pumpStream(byteArray: ByteArrayOutputStream, log: Logger)(processOutput: InputStream){

    try {
      val buffer = new Array[Byte](1024)
      var bytesRead: Int = 0
      do {
        bytesRead = processOutput.read(buffer)
        if (bytesRead > -1) {
          byteArray.write(buffer, 0, bytesRead)
        }
      } while (bytesRead > -1)
    }
    catch {
      case e: IOException => {
        log.error("Ignoring exception while reading output of process: " + e)
      }
    }
  }

  private def writeTo(input: Option[String])(stream: java.io.OutputStream) {
    input.foreach(v => {
      stream.write(v.getBytes())
      stream.flush()
    })
  }
}
