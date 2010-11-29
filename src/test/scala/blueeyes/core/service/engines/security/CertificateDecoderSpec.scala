package blueeyes.core.service.engines.security

import org.apache.commons.codec.binary.Base64
import org.specs.Specification
import EncoderUtil._

class CertificateDecoderSpec extends Specification with CertificateData{

  "CertificateKeyEntry must create key and certificate" in {
    val entry = CertificateDecoder(encodedPrivateKey, encodedCertificate)

    encode(entry._1.getEncoded) mustEqual (encodedPrivateKey)
    encode(entry._2.getEncoded) mustEqual (encodedCertificate)
  }

  private def encode(content: Array[Byte]) = unify(Base64.encodeBase64String(content))
}

class CertificateEncoderSpec extends Specification with CertificateData{

  "CertificateKeyEntry must create key and certificate" in {
    val entry   = CertificateDecoder(encodedPrivateKey, encodedCertificate)
    val encoded = CertificateEncoder(entry._1, entry._2)

    unify(encoded._1) mustEqual (encodedPrivateKey)
    unify(encoded._2) mustEqual (encodedCertificate)
  }
}

object EncoderUtil{
  def unify(value: String) = value.replaceAll("\r\n", "\n").trim
}