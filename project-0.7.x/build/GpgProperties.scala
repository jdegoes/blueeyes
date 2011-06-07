import java.io.{InputStreamReader, BufferedReader, FileInputStream, File}
import java.util.Properties
import sbt._

object GpgProperties{
  val password = {
    val file = (Path.userHome / ".gpg" / "gpg.security").asFile
    val value = if (file.exists){
      val stream = new FileInputStream(file)
      try {
        val properties = new Properties()
        properties.load(stream)
        val property = properties.getProperty("password")

        if (property != null) Some(property) else None
      }
      finally {if (stream != null) stream.close}

    } else None

    value.getOrElse(readPassword)
  }

  private def readPassword = {
    println("You need a passphrase to unlock the secret key.")
    print  ("Enter passphrase: ")
    readLine
  }
}