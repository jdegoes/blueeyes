package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpMethods._
import net.lag.configgy.{Config, ConfigMap}
import net.lag.logging.Logger

/**
 * An HttpServiceBuilder can be used to build any number of services. 
 * <p>
 * <pre>
 * trait EmailService extends HttpServiceBuilder[String] {
 *   val emailService = new service("email", "1.01") {
 *     path("/outgoing") {
 *       get { request =>
 *       }
 *     }
 *   }
 * }
 * </pre>
 */
trait HttpServiceBuilder[Base] { self =>
  import scala.collection.mutable.{ListBuffer, Stack}
  
  protected type RequestHandler[In, Out] = HttpRequest[In] => Future[HttpResponse[Out]]
  
  abstract class service(val name: String, override val version: String) extends HttpService2[Base] {
    override def startHooks       = _startHooks.toList
    override def stopHooks      = _stopHooks.toList
    override def notFoundHandler    = _notFoundHandler
    override def pathMethodHandlers = _pathMethodHandlers.toList
    
    private var _container: Option[HttpServicesContainer[Base]] = None
    private def container = _container.getOrElse(error("The service " + this.toString + " has not been installed into a container"))
    
    lazy val config: ConfigMap = container.rootConfig.configMap("services." + name + ".v" + majorVersion)
    
    lazy val log: Logger = Logger.configure(config.configMap("log"), false, false)
    
    def install(container: HttpServicesContainer[Base]) = {
      if (!_container.isEmpty) error("Service " + this.toString + " is already installed into " + container.toString)
      
      _container = Some(container)
    }
    
    def path(path: RestPathPattern)(f: => Unit): Unit = {
      pathStack.push(path)

      try { f } finally { pathStack.pop() }
    }

    def get[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(GET, handler)(in, out)

    def put[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(PUT, handler)(in, out)

    def post[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(POST, handler)(in, out)

    def delete[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(DELETE, handler)(in, out)

    def options[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(OPTIONS, handler)(in, out)

    def head[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(HEAD, handler)(in, out)

    def connect[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(CONNECT, handler)(in, out)

    def trace[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = custom(TRACE, handler)(in, out)

    def custom[In, Out](method: HttpMethod, handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]) = {
      _pathMethodHandlers += HttpPathMethodHandler(currentPath, method, handler, in, out)
    }

    def start(f: => Future[_]) = {
      val thunk = () => f

      _startHooks += thunk
    }

    def stop(f: => Future[_]) = {
      val thunk = () => f

      _stopHooks += thunk
    }

    def notFound[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): Unit = _notFoundHandler match {
      case None => _notFoundHandler = Some(HttpNotFoundHandler(handler, in, out))
      
      case _ => error("Not found handler already specified")
    }
    
    private val _startHooks       = new ListBuffer[() => Future[_]]
    private val _stopHooks      = new ListBuffer[() => Future[_]]
    private val _pathMethodHandlers = new ListBuffer[HttpPathMethodHandler[_, _, Base]]

    private var _notFoundHandler: Option[HttpNotFoundHandler[_, _, Base]] = None

    private val pathStack   = new Stack[RestPathPattern]
    private def currentPath = pathStack.foldRight[RestPathPattern](RestPathPattern.Root) { (element, path) => path / element }
  }
}