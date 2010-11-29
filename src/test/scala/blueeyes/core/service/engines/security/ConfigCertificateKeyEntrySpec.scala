package blueeyes.core.service.engines.security

import org.specs.Specification
import net.lag.configgy.Configgy
import org.apache.commons.codec.binary.Base64

class ConfigCertificateKeyEntrySpec extends Specification with CertificateData{

  private val configuration = """server {
  ssl{
    certificateEntry {
      privateKey = "%s"
      certificate = "%s"
  }
}""".format(encodedPrivateKey, encodedCertificate)

  "CertificateKeyEntry must create key and certificate" in {
    Configgy.configureFromString(configuration)
    
    val entry = ConfigCertificateKeyEntry(Configgy.config.configMap("server"))

    encode(entry.get._1.getEncoded) mustEqual (encodedPrivateKey)
    encode(entry.get._2.getEncoded) mustEqual (encodedCertificate)
  }
  "CertificateKeyEntry must not create key and certificate when configuration is missing" in {
    Configgy.configureFromString("")

    val entry = ConfigCertificateKeyEntry(Configgy.config.configMap("server"))

    entry must be(None)
  }

  private def encode(content: Array[Byte]) = Base64.encodeBase64String(content).replaceAll("\r\n", "\n").trim
}