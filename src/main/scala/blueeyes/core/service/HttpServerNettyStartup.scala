package blueeyes.core.service

import blueeyes.persistence.mongo.RealMongoModule
import blueeyes.config.{FilesystemConfiggyModule, ConfiggyModule}
import com.google.inject.Guice
import net.lag.configgy.Config

object HttpServerNettyStartup{
  private lazy val injector = Guice.createInjector(new FilesystemConfiggyModule(ConfiggyModule.FileLoc), new RealMongoModule)
  private lazy val configgy = injector.getInstance(classOf[Config])

  def main(args: Array[String]){
    val servicesName    = configgy.getList("server.services")
    val servicesClasses = servicesName.map(Class.forName(_).asInstanceOf[Class[HttpService[_ <: HttpService[_]]]])

    val serverNetty     = new HttpServerNetty(List(servicesClasses: _*))

    serverNetty.start(configgy.getInt("server.port").getOrElse(error("Server port is not configured. Congigure 'server.port'.")))

    Runtime.getRuntime()addShutdownHook(new Thread(new Runnable{
      def run = {
        serverNetty.stop        
      }
    }))
  }
}