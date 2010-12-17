package blueeyes.core.service

import blueeyes.health.HealthMonitor
import blueeyes.json.JPath
import net.lag.logging.Logger

trait HttpServiceDescriptorFactoryCombinators {
  def healthMonitor[T, S](f: HealthMonitor => HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val monitor = new HealthMonitor(JPath("%s.v%d.health".format(context.serviceName, context.serviceMajorVersion)))

      f(monitor)(context)
    }
  }

  def logging[T, S](f: Logger => HttpServiceDescriptorFactory[T, S]): HttpServiceDescriptorFactory[T, S] = {
    (context: HttpServiceContext) => {
      val logger = Logger.configure(context.config.configMap("log"), false, false)

      f(logger)(context)
    }
  }
}