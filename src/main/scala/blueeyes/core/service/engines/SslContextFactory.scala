package blueeyes.core.service.engines

import java.security.KeyStore
import javax.net.ssl.{TrustManager, KeyManagerFactory, SSLContext};




object SslContextFactory {
  def apply(keyStore: KeyStore, password: String, trustManagers: Option[Array[TrustManager]] = None) = {
    val keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
    keyManagerFactory.init(keyStore, password.toCharArray);

    val context = SSLContext.getInstance("TLS");
    context.init(keyManagerFactory.getKeyManagers(), trustManagers.getOrElse(null), null);

    context
  }
}