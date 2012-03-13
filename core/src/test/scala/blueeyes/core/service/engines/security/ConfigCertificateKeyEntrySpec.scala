package blueeyes.core.service.engines.security

import org.specs2.mutable.Specification
import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat
import org.apache.commons.codec.binary.Base64

class ConfigCertificateKeyEntrySpec extends Specification with CertificateData{
  override def is = args(sequential = true) ^ super.is

  private val configuration = """
server {
  ssl {
    certificateEntry {
      privateKey = "%s"
      certificate = "%s"
    }
  }
}
""".format(encodedPrivateKey, encodedCertificate)

  "CertificateKeyEntry must create key and certificate" in {
    val config = Configuration.parse(configuration, BlockFormat)
    
    val entry = ConfigCertificateKeyEntry(config.detach("server"))

    encode(entry.get._1.getEncoded) mustEqual (encodedPrivateKey)
    encode(entry.get._2.getEncoded) mustEqual (encodedCertificate)
  }
  "CertificateKeyEntry must not create key and certificate when configuration is missing" in {
    val config = Configuration.parse("", BlockFormat)

    val entry = ConfigCertificateKeyEntry(config.detach("server"))

    entry must be(None)
  }

  private def encode(content: Array[Byte]) = Base64.encodeBase64String(content).replaceAll("\r\n", "\n").trim
}
