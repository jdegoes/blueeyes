package blueeyes.core.service.engines.security

import net.lag.logging.Logger
import blueeyes.concurrent.Future
import java.lang.ProcessBuilder
import java.io.{InputStream, ByteArrayOutputStream}

object JavaKeyTool{

  private val log = Logger.get

  def apply(keystore: String, keyalg: String, alias: String, dname: String, validity: Int, password: String) = {
    val command = Array("keytool",
                        "-keystore",  keystore,
                        "-keyalg",    keyalg,
                        "-genkeypair",
                        "-alias",     alias,
                        "-dname",     dname,
                        "-validity",  validity.toString,
                        "-keypass",   password,
                        "-storepass", password)

    val processBuilder = new ProcessBuilder(command: _*)

    log.info("Creating keypair by command: " + command.mkString(" "))

    val process = processBuilder.start()

    val stdout = pump(process.getInputStream).deliverTo(log.info(_))
    val stderr = pump(process.getErrorStream).deliverTo(log.error(_))

    val exitCode = process.waitFor()

    log.info("JavaKeyTool finished with exit code: " + exitCode)

    (exitCode, stdout, stderr)
  }

  private def pump(is: InputStream): Future[String] = Future.async {
    val out = new ByteArrayOutputStream()

    var looping = true

    while (looping) {
      var b = is.read()

      if (b >= 0) {
        out.write(b)
      }
      else looping = false
    }

    out.toString("UTF-8")
  }
}
