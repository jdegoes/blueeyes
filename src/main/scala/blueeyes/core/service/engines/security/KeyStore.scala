package blueeyes.core.service.engines.security

import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import org.apache.commons.codec.binary.Base64
import net.lag.logging.Logger
import java.io.{FileInputStream, File, ByteArrayInputStream}
import java.security.cert.Certificate
import java.security.{Key, KeyStore, KeyFactory}
import blueeyes.concurrent.{FutureDeliveryStrategySequential, Future}

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

object KeyStoreFactory {
  def apply(key: Key, certificate: Certificate, alias: String, password: String) = {
    val passwordArray = password.toCharArray()
    val keystore      = KeyStore.getInstance("jks")

    keystore.load(null, passwordArray)

    keystore.setKeyEntry(alias, key, passwordArray, Array(certificate));

    keystore
  }
}

object JavaKeyTool extends FutureDeliveryStrategySequential{
  import java.lang.{Process, ProcessBuilder}
  import java.io.{InputStream, ByteArrayOutputStream}
  import scala.actors.Actor.actor
  
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
