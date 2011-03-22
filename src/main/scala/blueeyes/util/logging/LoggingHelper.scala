package blueeyes.util.logging

import net.lag.configgy.{ Config, ConfigMap }
import net.lag.logging.Logger

object LoggingHelper {
  def initializeLogging(config: ConfigMap, loggerName: String) = {
    val conf = if(config.contains("log")) {
      config.configMap("log")
    } else {
      val localConf = new Config()
      localConf.setString("level", "info")
      localConf.setBool("console", true)
      localConf.setBool("use_parents", false)
      localConf
    }

    // Always use passed name
    conf.setString("node", loggerName)

    Logger.configure(conf, false, true)
  }
}
