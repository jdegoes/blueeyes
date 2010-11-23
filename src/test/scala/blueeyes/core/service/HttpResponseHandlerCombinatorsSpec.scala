package blueeyes.core.service

import org.spex.Specification
import blueeyes.util.Future
import blueeyes.core.http._
import java.net.InetAddress

class HttpResponseHandlerCombinatorsSpec extends Specification with HttpResponseHandlerCombinators{
  private val initialRequest = HttpRequest[String](HttpMethods.GET, "")
  private val responseFuture = Future[String]("")

  "sets protocol, host, port and path to request" in{
    val (finalRequest, h) = protocol("https"){
      host("localhost"){
        port(8080){
          path("/foo"){
            path[String, String]("/bar")(handler)
          }
        }
      }
    }(initialRequest)
    
    finalRequest mustEqual(initialRequest.copy(uri = "https://localhost:8080/foo/bar"))
  }

  "sets parameters request" in{
    val (finalRequest, h) = parameters('foo -> "bar")(handler)(initialRequest)
    finalRequest mustEqual(initialRequest.copy(parameters = Map[Symbol, String]('foo -> "bar")))
  }
  "sets headers request" in{
    val (finalRequest, h) = headers("content-length" -> "1")(handler)(initialRequest)
    finalRequest mustEqual(initialRequest.copy(headers = Map[String, String]("content-length" -> "1")))
  }
  "sets remote host header request" in{
    val (finalRequest, h) = remoteHost(InetAddress.getLocalHost)(handler)(initialRequest)
    finalRequest mustEqual(initialRequest.copy(headers = Map[String, String]("X-Forwarded-For" -> InetAddress.getLocalHost.getHostAddress())))
  }
  "sets http version" in{
    val (finalRequest, h) = version(HttpVersions.`HTTP/1.0`)(handler)(initialRequest)
    finalRequest mustEqual(initialRequest.copy(version = HttpVersions.`HTTP/1.0`))
  }
  "sets request method" in{
    val (finalRequest, h) = get()((response: HttpResponse[String]) => responseFuture)(initialRequest)
    finalRequest mustEqual(initialRequest.copy(method = HttpMethods.GET))
  }

  private def handler = (request: HttpRequest[String]) => (request, (response: HttpResponse[String]) => responseFuture)

}