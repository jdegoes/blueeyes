package blueeyes.core.service.engines

import java.security.KeyStore;
import java.security.Security;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

//object SslContextFactory {
//
//  private val PROTOCOL = "TLS"
//  val serverContext =
//  {
//    String algorithm = Security.getProperty("ssl.KeyManagerFactory.algorithm");
//    if (algorithm == null) {
//        algorithm = "SunX509";
//    }
//
//    val ks = KeyStore.getInstance("JKS");
//    ks.load(SecureChatKeyStore.asInputStream(),
//            SecureChatKeyStore.getKeyStorePassword());
//
//    // Set up key manager factory to use our key store
//    val kmf = KeyManagerFactory.getInstance(algorithm);
//    kmf.init(ks, SecureChatKeyStore.getCertificatePassword());
//
//    // Initialize the SSLContext to work with our key managers.
//    val context = SSLContext.getInstance(PROTOCOL);
//    context.init(kmf.getKeyManagers(), null, null);
//
//    context
//  }
//}

//keytool -importkeystore -srckeystore foo.jks -destkeystore foo.p12 -srcstoretype jks -deststoretype pkcs12