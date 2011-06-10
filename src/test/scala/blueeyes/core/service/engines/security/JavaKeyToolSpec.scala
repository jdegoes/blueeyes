package blueeyes.core.service.engines.security

import org.specs.Specification
import java.io.File
import blueeyes.concurrent.FutureDeliveryStrategySequential

class JavaKeyToolSpec extends Specification with FutureDeliveryStrategySequential{
  "Creates keystore" in{
    val keyStorePath = System.getProperty("java.io.tmpdir") + "/" + System.currentTimeMillis + ".jks"
    val keyStore     = JavaKeyTool(keyStorePath, "RSA", "test", "CN=foo.example.com,L=Melbourne,ST=Victoria,C=AU", 10, "password")

    val keyStorePathFile = new File(keyStorePath)

    keyStorePathFile.exists must be (true)

    keyStorePathFile.delete
  }
}