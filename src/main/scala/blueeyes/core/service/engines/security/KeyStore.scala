package blueeyes.core.service.engines.security

import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import org.apache.commons.codec.binary.Base64
import net.lag.logging.Logger
import java.io.{FileInputStream, File, ByteArrayInputStream}
import java.security.cert.Certificate
import java.security.{Key, KeyStore, KeyFactory}

object CertificateDecoder{
  def apply(encodedPrivateKey: String, encodedCertificate: String) = {
    val keyFactory = KeyFactory.getInstance("DSA")
    val keySpec    = new PKCS8EncodedKeySpec(Base64.decodeBase64(encodedPrivateKey))
    val privateKey = keyFactory.generatePrivate(keySpec)    

    val certificateFactory = CertificateFactory.getInstance("X.509")
    val certificate        = certificateFactory.generateCertificate(new ByteArrayInputStream(Base64.decodeBase64(encodedCertificate)))

    Tuple2(privateKey, certificate)
  }
}

object CertificateEncoder{
  def apply(key: Key, certificate: Certificate) = Tuple2(Base64.encodeBase64String(key.getEncoded), Base64.encodeBase64String(certificate.getEncoded)) 
}

object KeyStoreFactory{
  def apply(key: Key, certificate: Certificate, alias: String, password: String) = {
    val passwordArray = password.toCharArray()
    val keystore      = KeyStore.getInstance("jks")

    keystore.load(null, passwordArray)

    keystore.setKeyEntry(alias, key, passwordArray, Array(certificate));

    keystore
  }
}

import scala.tools.nsc.io.Process
object JavaKeyTool{
  private val log = Logger.get
  def apply(keystore: String, keyalg: String, alias: String, dname: String, validity: Int, password: String) = {
    val command = "keytool -keystore %s -keyalg %s -genkeypair -alias %s -dname '%s' -validity %d -keypass %s -storepass %s".format(keystore, keyalg, alias, dname, validity, password, password)

    log.info("Creating keypair by command: " + command)

    val process = Process(command)

    log.info("JavaKeyTool finished with exit code: " + process.exitValue)
    process.stdout.foreach(log.info(_))
    process.stderr.foreach(log.error(_))    

    Tuple3(process.exitValue, process.stdout, process.stderr)
  }
}
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
