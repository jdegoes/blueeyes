package blueeyes.core.service.engines.security

import net.lag.configgy.ConfigMap
import java.security.KeyStore
import blueeyes.core.service.engines.InetInrerfaceLookup

object CertificateConfig{
  val SslKey              = "ssl"
  val CertificateEntryKey = "certificateEntry"
  val CertificateKey      = "certificate"
  val PrivateKeyKey       = "privateKey"
}

import CertificateConfig._
object ConfigCertificateKeyEntry{
  def apply(config: ConfigMap) = {
    val certificateConfig   = config.configMap(SslKey).configMap(CertificateEntryKey)

    val privateKey  = certificateConfig.getString(PrivateKeyKey)
    val certificate = certificateConfig.getString(CertificateKey)

    val keyAndCertificate = privateKey.flatMap(keyValue => certificate.map(certificateValue => Tuple2(keyValue, certificateValue)))
    keyAndCertificate.map(v => CertificateDecoder(v._1, v._2))
  }
}

object BlueEyesKeyStoreFactory{
  val alias    = "SSL-Support"
  val password = "SSL-Support2009"
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var keyStore: Option[KeyStore] = None

  def apply(config: ConfigMap) = {
    writeLock{
      val value = keyStore.getOrElse({
        val newKeyStore = create(config)
        keyStore        = Some(newKeyStore)
        newKeyStore
      })
      value
    }
  }

  private def create(config: ConfigMap) = {
    val keyAndCertificate = ConfigCertificateKeyEntry(config).getOrElse(generate(config))

    KeyStoreFactory(keyAndCertificate._1, keyAndCertificate._2, alias, password)
  }

  private def generate(config: ConfigMap) = {
    val certificateConfig   = config.configMap(SslKey).configMap(CertificateEntryKey)
    val cn = "CN=" + certificateConfig.getString("CN", InetInrerfaceLookup.host(config))
    CertificateGenerator("RSA", alias, cn, 36500, password)
  }

  private def writeLock[S](f: => S): S = {
    lock.writeLock.lock()
    try {
      f
    }
    finally {
      lock.writeLock.unlock()
    }
  }
}
