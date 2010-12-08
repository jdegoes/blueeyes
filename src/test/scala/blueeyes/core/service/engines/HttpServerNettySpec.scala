package blueeyes.core.service.engines

import com.ning.http.client._
import blueeyes.core.service._
import org.specs.Specification
import blueeyes.util.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.BlueEyesServiceBuilderString
import net.lag.configgy.Configgy
import java.util.concurrent.CountDownLatch
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import security.BlueEyesKeyStoreFactory
import javax.net.ssl.TrustManagerFactory


class HttpServerNettySpec extends Specification {

  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""

  shareVariables()

  private var port = 8585
  private var server: Option[NettyEngineString] = None
  private var clientFacade: SampleClientFacade = _
  
  "HttpServer" should{
    doFirst{
      var success = false
      do{
        SampleServer.sampleService
        success = try {
          val doneSignal = new CountDownLatch(1)

          Configgy.configureFromString(configPattern.format(port, port + 1))

          SampleServer.start.deliverTo { _ => doneSignal.countDown()}
          true
        }
        catch {
          case e: Throwable => {
            e.printStackTrace()
            port = port + 2
            false
          }
        }
      }while(!success)

      clientFacade = new SampleClientFacade(port, port + 1)
      server       = Some(SampleServer)
    }

    "return html by correct URI by https" in{
      val response =  LocalHttpsClient.execSynch(clientFacade.httpsRequest)
      response.content.get mustEqual (Context.context)
    }

    "return html by correct URI" in{
      val response =  LocalHttpsClient.execSynch(clientFacade.httpRequest)

      response.status       mustEqual (HttpStatus(OK))
      response.content.get  mustEqual (Context.context)
    }

    "return not found error by wrong URI" in{
      val response = LocalHttpsClient.execSynch(clientFacade.wrongHttpRequest)

      response.status mustEqual (HttpStatus(NotFound))
    }
    "return Internall error when handling request crushes" in{
      val response = LocalHttpsClient.execSynch(clientFacade.errorHttpRequest)

      response.status mustEqual (HttpStatus(InternalServerError))
    }
    "return Http error when handling request throws HttpException" in{
    val response = LocalHttpsClient.execSynch(clientFacade.httpErrorHttpRequest)
      response.status mustEqual (HttpStatus(BadRequest))
    }

    "return html by correct URI with parameters" in{
      val response = LocalHttpsClient.execSynch(clientFacade.httpRequestWithParams)

      response.status       mustEqual (HttpStatus(OK))
      response.content.get  mustEqual (Context.context)
    }

    doLast{
      server.foreach(_.stop)
    }
  }
}

object SampleServer extends SampleService with HttpReflectiveServiceList[String] with NettyEngineString { }

object LocalHttpsClient extends HttpClientXLightWebEnginesString{
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(SampleServer.config)
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
  def httpsRequest = protocol("https"){
    host("localhost"){
      port(sslPort){
        path("/bar/foo/adCode.html"){
          get[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpRequest = protocol("http"){
    host("localhost"){
      port(port){
        path("/bar/foo/adCode.html"){
          get[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpRequestWithParams = protocol("http"){
    host("localhost"){
      port(port){
        path("/foo"){
          parameters('bar -> "zar"){
            get[String, HttpResponse[String]]{ simpleHandler }
          }
        }
      }
    }
  }
  def wrongHttpRequest = protocol("http"){
    host("localhost"){
      port(port){
        path("/foo/foo/adCode.html"){
          post[String, HttpResponse[String]]("foo"){ simpleHandler }
        }
      }
    }
  }
  def errorHttpRequest = protocol("http"){
    host("localhost"){
      port(port){
        path("/error"){
          get[String, HttpResponse[String]]{ simpleHandler }
        }
      }
    }
  }
  def httpErrorHttpRequest = protocol("http"){
    host("localhost"){
      port(port){
        path("/http/error"){
          get[String, HttpResponse[String]]{ simpleHandler }
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
