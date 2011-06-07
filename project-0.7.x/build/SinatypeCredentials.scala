import org.apache.ivy.util.url.CredentialsStore

object SinatypeCredentials{
  val credentials = CredentialsStore.INSTANCE.getCredentials("Sonatype Nexus Repository Manager", "oss.sonatype.org")
}