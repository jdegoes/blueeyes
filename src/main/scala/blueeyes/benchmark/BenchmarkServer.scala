package blueeyes.benchmark

import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch
import blueeyes.BlueEyesServer
import blueeyes.core.service.ServerHealthMonitorService
import blueeyes.persistence.mongo.RealMongo
import blueeyes.core.service.engines.HttpClientXLightWeb
import blueeyes.demo.{BlueEyesDemoFacade, BlueEyesDemoService, BlueEyesDemo}

object BenchmarkServerStart extends ServerStart{
  def main(args: Array[String]) = startServer(if (args.size > 0) args(0).toBoolean else false)
}

object LiveBlueEyesDemo extends BlueEyesServer with BlueEyesDemoService with ServerHealthMonitorService{
  lazy val mongo = {
    Configgy.configure("/etc/default/blueeyes.conf")
    new RealMongo(Configgy.config.configMap("mongo"))
  }
  override def main(args: Array[String]) = super.main(Array("--configFile", "/etc/default/blueeyes.conf"))
}

trait ServerStart{ self =>
  private var server: Option[BlueEyesServer] = _
  var port = 8585
  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""
  def startServer(liveDemo: Boolean){
    var error: Option[Throwable] = None
    do{
      val doneSignal   = new CountDownLatch(1)

      Configgy.configureFromString(configPattern.format(port, port + 1))

      server = Some(if (liveDemo) LiveBlueEyesDemo else BlueEyesDemo)
      val startFuture = server.get.start
      startFuture.deliverTo { _ =>
        error = None
        doneSignal.countDown()
      }
      startFuture.ifCanceled{v =>
        error = v
        port  = port + 2
        doneSignal.countDown()
      }
    }while(error != None)

    error.foreach(throw _)
  }

  def stopServer = server.foreach(_.stop)

  class BlueEyesDemoFacadeImpl extends BlueEyesDemoFacade{
    def port = self.port
    val httpClient = new HttpClientXLightWeb
  }
}
