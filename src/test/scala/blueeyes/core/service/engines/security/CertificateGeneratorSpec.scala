package blueeyes.core.service.engines.security

import org.specs.Specification

class CertificateGeneratorSpec extends Specification{
  "Creates key and certificate" in{
    val keyAndCertificate     = CertificateGenerator("RSA", "test", "CN=foo.example.com,L=Melbourne,ST=Victoria,C=AU", 36500, "password")

    keyAndCertificate must not be (null)
  }
}