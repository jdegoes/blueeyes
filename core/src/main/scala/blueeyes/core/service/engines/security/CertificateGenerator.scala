package blueeyes.core.service.engines.security

import java.io.{File, FileInputStream}
import java.security.KeyStore

object CertificateGenerator{

  def apply(keyalg: String, alias: String, dname: String, validity: Int, password: String) = {
    val keyStorePath  = System.getProperty("java.io.tmpdir") + "/" + System.currentTimeMillis + ".jks"

    JavaKeyTool(keyStorePath, keyalg, alias, dname, validity, password)

    val keyStorePathFile = new File(keyStorePath)

    if (!keyStorePathFile.exists) throw new Exception("Certificate cannot be created.")

    val keystoreStream = new FileInputStream(keyStorePathFile)

    try {
      val keystore = KeyStore.getInstance("jks")

      val passwordArray = password.toCharArray()
      keystore.load(keystoreStream, passwordArray)

      Tuple2(keystore.getKey(alias, passwordArray), keystore.getCertificate(alias))
    }
    finally {
      keystoreStream.close
      keyStorePathFile.delete
    }
  }
}
