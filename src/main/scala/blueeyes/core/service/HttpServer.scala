package blueeyes.core.service

import java.lang.reflect.{Method, Field}
import scala.collection.mutable.{ListBuffer}

import blueeyes.util.Future
import blueeyes.util.CommandLineArguments
import net.lag.configgy.{Config, ConfigMap, Configgy}
import net.lag.logging.Logger

/** An http server acts as a container for services. A server can be stopped
 * and started, and has a main function so it can be mixed into objects.
 */
trait HttpServer[T] extends HttpServicesContainer[T] { self =>
  var rootConfig: Config = null
  
  lazy val services: List[HttpService2[T]] = {
    val c = self.getClass
    
    val allMethods: List[Method] = c.getDeclaredMethods.toList
    
    val serviceFields: List[Field] = c.getDeclaredFields.toList.reverse.filter { field => 
      classOf[HttpService2[T]].isAssignableFrom(field.getType) &&
      allMethods.exists(m => field.getName == m.getName && m.getReturnType == field.getType)
    }
    
    val services: List[HttpService2[T]] = serviceFields.map { field =>
      field.get(self).asInstanceOf[HttpService2[T]]
    }
     
    //serviceFields.map(_.get(self))
    
    services
  }
  
  /** Starts the server.
   */
  def start: Future[Unit]
  
  /** Stops the server.
   */
  def stop: Future[Unit]
  
  /** Retrieves the server configuration, which is always located in the 
   * "server" block of the root configuration object.
   */
  lazy val config: ConfigMap = rootConfig.configMap("server")
  
  /** Retrieves the logger for the server, which is configured directly from
   * the server's "log" configuration block.
   */
  lazy val log: Logger = Logger.configure(config.configMap("log"), false, false)
  
  /** Retrieves the port the server should be running at, which defaults to
   * 8888.
   */
  lazy val port: Int = config.getInt("port", 8888)
  
  def installServices = services.foreach { s => s.install(this) }
  
  /** A default main function, which accepts the configuration file from the
   * command line, with flag "--configFile".
   */
  def main(args: Array[String]) = {
    val arguments = CommandLineArguments(args: _*)
    
    if (arguments.size == 0 || !arguments.parameters.get("help").isEmpty) {
      println("Usage: --configFile [config file]")
      
      System.exit(-1)
    }
    else {    
      Configgy.configure(arguments.parameters.get("configFile").getOrElse(error("Expected --configFile option")))
      
      rootConfig = Configgy.config
      
      start.deliverTo { _ =>
        println("Server started")
        
        Runtime.getRuntime.addShutdownHook { new Thread {
          override def start() {
            import java.util.concurrent.CountDownLatch
            
            val doneSignal = new CountDownLatch(1);
            
            self.stop.deliverTo { _ => doneSignal.countDown() }
            
            doneSignal.await()
            
            println("Server stopped")
          }
        }}
      }
    }
  }
}