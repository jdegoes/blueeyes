package blueeyes.util.logging

import net.lag.configgy.{ Config, ConfigMap }
import net.lag.logging.Logger

object LoggingHelper {
  def initializeLogging(config: ConfigMap, defaultName: String) = {
    val conf = if(config.contains("log")) {
      config.configMap("log")
    } else {
      val localConf = new Config()
      localConf.setString("node", defaultName)
      localConf.setString("level", "info")
      localConf.setBool("console", true)
      localConf.setBool("use_parents", false)
      localConf
    }

    Logger.configure(conf, false, true)
  }
}
