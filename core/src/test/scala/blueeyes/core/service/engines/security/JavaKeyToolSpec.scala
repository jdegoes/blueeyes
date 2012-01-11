package blueeyes.core.service.engines.security

import org.specs2.mutable.Specification
import java.io.File

class JavaKeyToolSpec extends Specification{
  "Creates keystore" in{
    val keyStorePath = System.getProperty("java.io.tmpdir") + "/" + System.currentTimeMillis + ".jks"
    val keyStore     = JavaKeyTool(keyStorePath, "RSA", "test", "CN=foo.example.com,L=Melbourne,ST=Victoria,C=AU", 10, "password")

    val keyStorePathFile = new File(keyStorePath)

    keyStorePathFile.exists must be_==(true)

    keyStorePathFile.delete
  }
}