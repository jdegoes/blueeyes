package blueeyes.core.service

import org.specs2.mutable.Specification

import blueeyes.bkka._
import blueeyes.core.http._
import blueeyes.core.data._
import DefaultBijections._

import akka.dispatch.Future

import java.net.InetAddress
import java.nio.ByteBuffer

import org.jboss.netty.handler.codec.http.CookieEncoder

class HttpClientSpec extends Specification with TestAkkaDefaults {

  override def is = args(sequential = true) ^ super.is

  private val initialRequest = HttpRequest[String](HttpMethods.GET, "/baz")

  private val mockClient = new HttpClient[String]{
    var request: Option[HttpRequest[String]] = None

    def apply(r: HttpRequest[String]) = {
      request = Some(r)
      Future[HttpResponse[String]](HttpResponse[String]())
    }

    def isDefinedAt(x: HttpRequest[String]) = true
  }

  "sets protocol, host, port and path to request" in {  
    makeTest(HttpRequest[String](HttpMethods.GET, "https://google.com:8080/bar/foo" + initialRequest.uri)) {
      client => client.host("google.com").path("/bar/foo").protocol("https").port(8080)
    }
  }

  "sets uri and method" in { 
    makeTest(HttpRequest[String](HttpMethods.DELETE, "foo-foo"), "foo-foo", HttpMethods.DELETE) {
      client => client
    }
  }

  "sets path" in {  
    makeTest(HttpRequest[String](HttpMethods.GET, "http://google.com" +  initialRequest.uri)) {
      client => client.path("http://google.com")
    } 
  }

  "sets content" in{  makeTest(initialRequest.copy(content = Some("1"))) {client => client.content("1".getBytes)} }

  "sets cookies" in{
    val cookieEncoder = new CookieEncoder(false)
    cookieEncoder.addCookie("foo", "bar")

    makeTest(initialRequest.copy(headers = initialRequest.headers + Tuple2("Cookie", cookieEncoder.encode()))) {client => client.cookies(("foo", "bar"))}
  }

  "sets parameters request" in{  makeTest(initialRequest.copy(parameters = Map[Symbol, String]('foo -> "bar"))) {client => client.parameters('foo -> "bar")} }

  "sets query" in{  makeTest(HttpRequest[String](HttpMethods.GET, initialRequest.uri + "?foo=bar", Map('foo -> "bar"))) {client => client.query("foo", "bar")} }

  "sets http version" in{ makeTest(initialRequest.copy(version = HttpVersions.`HTTP/1.0`)) {client => client.version(HttpVersions.`HTTP/1.0`)} }

  "sets headers request" in{ makeTest(initialRequest.copy(headers = Map[String, String]("Content-Length" -> "1"))) {client => client.header("content-length", "1")} }

  "sets remote host header request" in{ makeTest(initialRequest.copy(headers = Map[String, String]("X-Forwarded-For" -> InetAddress.getLocalHost.getHostAddress(), "X-Cluster-Client-Ip" -> InetAddress.getLocalHost.getHostAddress()),
      remoteHost = Some(InetAddress.getLocalHost))) {client => client.remoteHost(InetAddress.getLocalHost)} }

  private def makeTest(expectation: HttpRequest[String], uri: URI = initialRequest.uri, method: HttpMethod = initialRequest.method)(builder: (HttpClient[String]) => HttpClient[String]) = {
    builder(mockClient).custom[String](method, uri.toString)

    mockClient.request.get mustEqual(expectation)
  }
}
