import java.io.{FileInputStream, File}
import java.util.Properties
import sbt._

object GpgProperties{
  lazy val password = {
    val default = SinatypeCredentials.credentials.getPasswd
    val file = (Path.userHome / ".gpg" / "gpg.security").asFile
    if (file.exists){
      val stream = new FileInputStream(file)
      try {
        val properties = new Properties()
        properties.load(stream)
        properties.getProperty("password", default)
      }
      finally {if (stream != null) stream.close}

    } else default
  }
}