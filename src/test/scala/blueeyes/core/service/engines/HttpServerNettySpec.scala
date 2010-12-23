package blueeyes.core.service.engines

import com.ning.http.client._
import blueeyes.core.service._
import org.specs.Specification
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory
import javax.net.ssl.TrustManagerFactory
import net.lag.configgy.{ConfigMap, Configgy}


class HttpServerNettySpec extends Specification {

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  shareVariables()

  private var port = 8585
  private var server: Option[NettyEngineString] = None
  private var clientFacade: SampleClientFacade = _
  private var client: LocalHttpsClient = _

  "HttpServer" should{
    doFirst{
      var error: Option[Throwable] = None
      do{
        val sampleServer = new SampleServer()
        val doneSignal   = new CountDownLatch(1)

        Configgy.configureFromString(configPattern.format(port, port + 1))

        val startFuture = sampleServer.start
        startFuture.deliverTo { _ =>
          error = None
          doneSignal.countDown()
        }
        startFuture.ifCanceled{v =>
          error = v
          port  = port + 2
          doneSignal.countDown()
        }

        server       = Some(sampleServer)        
      }while(error != None)

      clientFacade = new SampleClientFacade(port, port + 1)
      client       = new LocalHttpsClient(server.get.config)
    }

    "return html by correct URI by https" in{
      val response =  client.execSynch(clientFacade.httpsRequest)
      response.content.get mustEqual (Context.context)
    }

    "return html by correct URI" in{
      val response =  client.execSynch(clientFacade.httpRequest)

      response.status       mustEqual (HttpStatus(OK))
      response.content.get  mustEqual (Context.context)
    }

    "return not found error by wrong URI" in{
      val response = client.execSynch(clientFacade.wrongHttpRequest)

      response.status mustEqual (HttpStatus(NotFound))
    }
    "return Internall error when handling request crushes" in{
      val response = client.execSynch(clientFacade.errorHttpRequest)

      response.status mustEqual (HttpStatus(InternalServerError))
    }
    "return Http error when handling request throws HttpException" in{
    val response = client.execSynch(clientFacade.httpErrorHttpRequest)
      response.status mustEqual (HttpStatus(BadRequest))
    }

    "return html by correct URI with parameters" in{
      val response = client.execSynch(clientFacade.httpRequestWithParams)

      response.status       mustEqual (HttpStatus(OK))
      response.content.get  mustEqual (Context.context)
    }

    doLast{
      server.foreach(_.stop)
    }
  }
}

class SampleServer extends SampleService with HttpReflectiveServiceList[String] with NettyEngineString { }

class LocalHttpsClient(config: ConfigMap) extends HttpClientXLightWebEnginesString{
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(config)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    trustManagerFactory.init(keyStore)

    SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
  }

  def execSynch[R](f: HttpClient[String] => Future[R]): R = {
    val future = f(this)

    val counDown = new CountDownLatch(1)
    future.deliverTo(response =>{
      counDown.countDown
    })
    counDown.await
    future.value.get
  }
}

class SampleClientFacade(port: Int, sslPort: Int) extends HttpClientTransformerCombinators{
  def httpsRequest = protocol$("https"){
    host$("localhost"){
      port$(sslPort){
        path$("/bar/foo/adCode.html"){
          get$[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpRequest = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/bar/foo/adCode.html"){
          get$[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpRequestWithParams = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/foo"){
          parameters$('bar -> "zar"){
            get$[String, HttpResponse[String]]{ simpleHandler }
          }
        }
      }
    }
  }
  def wrongHttpRequest = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/foo/foo/adCode.html"){
          post$[String, HttpResponse[String]]("foo"){ simpleHandler }
        }
      }
    }
  }
  def errorHttpRequest = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/error"){
          get$[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpErrorHttpRequest = protocol$("http"){
    host$("localhost"){
      port$(port){
        path$("/http/error"){
          get$[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }

  private def simpleHandler = (response: HttpResponse[String]) => new Future[HttpResponse[String]]().deliver(response)
}

trait SampleService extends BlueEyesServiceBuilderString {
  import blueeyes.core.http.MimeTypes._
  
  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(Context.context))
  
  val sampleService: HttpService[String] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }
        } ~ 
        path("/foo") {
          get { request: HttpRequest[String] =>
            new Future[HttpResponse[String]]().deliver(response)
          }  
        } ~
        path("/error") {
          get { request: HttpRequest[String] =>
            throw new RuntimeException("Unexecpcted Error.")
          }
        } ~
        path("/http/error") {
          get { request: HttpRequest[String] =>
            throw HttpException(HttpStatusCodes.BadRequest)
          }
        }
      }
    }
  }
}

object Context{
  val context = """<html>
<head>
</head>

<body>
    <h1>Test</h1>
</body>
</html>"""
}
