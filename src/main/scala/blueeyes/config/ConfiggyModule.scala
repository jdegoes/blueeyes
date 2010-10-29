package blueeyes.config

import net.lag.configgy.{Configgy, Config}
import com.google.inject.AbstractModule

object ConfiggyModule {
  val FileLoc = "/etc/default/blueeyes.conf"
}

class ConfiggyModule(config: Config) extends AbstractModule {
  override protected def configure(): Unit = {
    bind(classOf[Config]).toInstance(config)
  }
}

class FilesystemConfiggyModule(fileLoc: String) extends AbstractModule {
  import ConfiggyModule._

  override protected def configure(): Unit = {
    Configgy.configure(fileLoc)
    bind(classOf[Config]).toInstance(Configgy.config)
  }
}