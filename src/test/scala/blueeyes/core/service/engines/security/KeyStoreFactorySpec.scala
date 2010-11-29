package blueeyes.core.service.engines.security

import org.specs.Specification

class KeyStoreFactorySpec extends Specification with CertificateData{
  "KeyStoreFactory creates KeyStore" in{
    val data     = CertificateDecoder(encodedPrivateKey, encodedCertificate)
    val keyStore = KeyStoreFactory(data._1, data._2, "foo", "password")

    keyStore.getCertificate("foo") must not be (null)
    keyStore.getKey("foo", "password".toCharArray()) must not be (null)
  }
}