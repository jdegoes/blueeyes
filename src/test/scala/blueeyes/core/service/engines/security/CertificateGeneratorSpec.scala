package blueeyes.core.service.engines.security

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class CertificateGeneratorSpec extends Specification with MustThrownMatchers{
  "Creates key and certificate" in{
    val keyAndCertificate     = CertificateGenerator("RSA", "test", "CN=foo.example.com,L=Melbourne,ST=Victoria,C=AU", 36500, "password")

    keyAndCertificate must not be (null)
  }
}