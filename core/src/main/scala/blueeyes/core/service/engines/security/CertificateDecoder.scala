package blueeyes.core.service.engines.security

import java.security.cert.Certificate
import java.security.{KeyFactory, Key}
import java.io.ByteArrayInputStream
import java.security.spec.PKCS8EncodedKeySpec
import java.security.cert.CertificateFactory
import org.apache.commons.codec.binary.Base64

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
