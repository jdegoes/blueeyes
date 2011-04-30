package blueeyes.core.service.engines.security

import java.security.cert.Certificate
import java.security.{Key, KeyStore}

object KeyStoreFactory {
  def apply(key: Key, certificate: Certificate, alias: String, password: String) = {
    val passwordArray = password.toCharArray()
    val keystore      = KeyStore.getInstance("jks")

    keystore.load(null, passwordArray)

    keystore.setKeyEntry(alias, key, passwordArray, Array(certificate));

    keystore
  }
}
