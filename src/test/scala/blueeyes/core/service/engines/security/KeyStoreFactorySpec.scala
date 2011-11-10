package blueeyes.core.service.engines.security

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class KeyStoreFactorySpec extends Specification with CertificateData with MustThrownMatchers{
  "KeyStoreFactory creates KeyStore" in{
    val data     = CertificateDecoder(encodedPrivateKey, encodedCertificate)
    val keyStore = KeyStoreFactory(data._1, data._2, "foo", "password")

    keyStore.getCertificate("foo") must not be (null)
    keyStore.getKey("foo", "password".toCharArray()) must not be (null)
  }
}