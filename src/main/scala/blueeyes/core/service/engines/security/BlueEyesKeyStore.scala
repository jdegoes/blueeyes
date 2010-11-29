package blueeyes.core.service.engines.security

import net.lag.configgy.ConfigMap

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
