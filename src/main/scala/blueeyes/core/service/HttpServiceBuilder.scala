package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpMethods._
import net.lag.configgy.{Config, ConfigMap}
import net.lag.logging.Logger

/**
 * An HttpServiceBuilder can be used to build any number of services. The trait
 * should ultimately be mixed into an HttpServicesContainer implementation.
 * <p>
 * <pre>
 * trait EmailService extends HttpServiceBuilder[String] {
 *   new service("email", "1.01") {
 *     path("/outgoing") {
 *       get { request =>
 *       }
 *     }
 *   }
 * }
 * </pre>
 */
trait HttpServiceBuilder[Base] extends HttpServicesContainer[Base] { self =>
  import scala.collection.mutable.{ListBuffer, Stack}
  
  protected type RequestHandler[In, Out] = HttpRequest[In] => Future[HttpResponse[Out]]
  
  case class service(name: String, override val version: String) extends HttpService2[Base] {
    self.services += this
    
    override def startupHooks       = _startupHooks.toList
    override def shutdownHooks      = _shutdownHooks.toList
    override def notFoundHandler    = _notFoundHandler
    override def pathMethodHandlers = _pathMethodHandlers.toList
    
    lazy val config: ConfigMap = self.rootConfig.configMap("services." + name + "." + majorVersion)
    
    lazy val log: Logger = Logger.configure(config.configMap("log"), false, false)
    
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

    def startup(f: => Future[_]) = {
      val thunk = () => f

      _startupHooks += thunk
    }

    def shutdown(f: => Future[_]) = {
      val thunk = () => f

      _shutdownHooks += thunk
    }

    def notFound[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): Unit = _notFoundHandler match {
      case None => _notFoundHandler = Some(HttpNotFoundHandler(handler, in, out))
      
      case _ => error("Not found handler already specified")
    }
    
    private val _startupHooks       = new ListBuffer[() => Future[_]]
    private val _shutdownHooks      = new ListBuffer[() => Future[_]]
    private val _pathMethodHandlers = new ListBuffer[HttpPathMethodHandler[_, _, Base]]

    private var _notFoundHandler: Option[HttpNotFoundHandler[_, _, Base]] = None

    private val pathStack   = new Stack[RestPathPattern]
    private def currentPath = pathStack.foldRight[RestPathPattern](RestPathPattern.Root) { (element, path) => path / element }
  }
}