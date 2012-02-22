package blueeyes.core.service.engines.security

import org.streum.configrity.Configuration

import java.security.KeyStore
import blueeyes.core.service.engines.InetInterfaceLookup

object CertificateConfig{
  val SslKey              = "ssl"
  val CertificateEntryKey = "certificateEntry"
  val CertificateKey      = "certificate"
  val PrivateKeyKey       = "privateKey"
}

import CertificateConfig._
object ConfigCertificateKeyEntry{
  def apply(config: Configuration) = {
    val certificateConfig   = config.detach(SslKey).detach(CertificateEntryKey)

    val privateKey  = certificateConfig.get[String](PrivateKeyKey)
    val certificate = certificateConfig.get[String](CertificateKey)

    val keyAndCertificate = privateKey.flatMap(keyValue => certificate.map(certificateValue => Tuple2(keyValue, certificateValue)))
    keyAndCertificate.map(v => CertificateDecoder(v._1, v._2))
  }
}

object BlueEyesKeyStoreFactory{
  val alias    = "SSL-Support"
  val password = "SSL-Support2009"
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  private var keyStore: Option[KeyStore] = None

  def apply(config: Configuration) = {
    writeLock{
      val value = keyStore.getOrElse({
        val newKeyStore = create(config)
        keyStore        = Some(newKeyStore)
        newKeyStore
      })
      value
    }
  }

  private def create(config: Configuration) = {
    val keyAndCertificate = ConfigCertificateKeyEntry(config).getOrElse(generate(config))

    KeyStoreFactory(keyAndCertificate._1, keyAndCertificate._2, alias, password)
  }

  private def generate(config: Configuration) = {
    val certificateConfig   = config.detach(SslKey).detach(CertificateEntryKey)
    val cn = "CN=" + certificateConfig[String]("CN", InetInterfaceLookup.host(config))
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
