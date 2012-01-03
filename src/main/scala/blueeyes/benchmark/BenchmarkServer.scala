package blueeyes.benchmark

import akka.util.Timeout

import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch
import blueeyes.BlueEyesServer
import blueeyes.core.service.ServerHealthMonitorService
import blueeyes.persistence.mongo.RealMongo
import blueeyes.core.service.engines.HttpClientXLightWeb
import blueeyes.demo.{BlueEyesDemoFacade, BlueEyesDemoService, BlueEyesDemo}
import com.weiglewilczek.slf4s.Logging

object BenchmarkServerStart extends BenchmarkServer {
  def main(args: Array[String]) = startServer(if (args.size > 0) args(0).toBoolean else false)
}

object LiveBlueEyesDemo extends BlueEyesServer with BlueEyesDemoService with ServerHealthMonitorService {
  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait BenchmarkServer extends Logging { self =>
  private var server: Option[BlueEyesServer] = _

  var port = 8585

  private val configPattern = """server{
    port = %d
    sslPort = %d
  }"""

  def startServer(liveDemo: Boolean) {
    def start(port: Int) {
      Configgy.configureFromString(configPattern.format(port, port + 1))

      server = Some(if (liveDemo) LiveBlueEyesDemo else BlueEyesDemo)
      server.get.start onFailure { 
        case v =>
          logger.error("Server failed to start, trying port " + (port + 2))
          start(port + 2)
      }
    } 

    start(port)
  }

  def stopServer = server.foreach(_.stop)

  class BlueEyesDemoFacadeImpl extends BlueEyesDemoFacade{
    def port = self.port
    val httpClient = new HttpClientXLightWeb
  }
}
