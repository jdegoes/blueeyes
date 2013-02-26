package blueeyes.core.http

import blueeyes.core.http.HttpVersions._
import blueeyes.json._
import HttpHeaders._

import java.net.InetAddress
import scalaz._

sealed case class HttpRequest[T] private(method: HttpMethod, uri: URI, parameters: Map[Symbol, String], headers: HttpHeaders, content: Option[T], remoteHost: Option[InetAddress], version: HttpVersion, subpath: String) {

  lazy val mimeTypes: List[MimeType] = (for (`Content-Type`(mimeTypes) <- headers.raw) yield mimeTypes.toList).toList.flatten

  def withSubpath(p: String) = copy(subpath = p)

  def withUriChanges(scheme: Option[String] = this.uri.scheme, userInfo: Option[String] = this.uri.userInfo, host: Option[String] = this.uri.host, port: Option[Int] = this.uri.port, path: Option[String] = this.uri.path, query: Option[String] = this.uri.query, fragment: Option[String] = this.uri.fragment) = {
    val newUri = URI(scheme, userInfo, host, port, path, query, fragment)

    copy(uri = newUri)
  }

  def map[U](f: T => U): HttpRequest[U] = copy(content = content map f)
}

object HttpRequest{
  def apply[T](method: HttpMethod, uri: URI, parameters: Map[Symbol, String] = Map(), headers: HttpHeaders = HttpHeaders.Empty, content: Option[T] = None, remoteHost: Option[InetAddress] = None, version: HttpVersion = `HTTP/1.1`): HttpRequest[T] = {
    val subpath   = uri.path.getOrElse("")
    val query     = uri.query
    val allParameters = parameters ++ query.map(v => blueeyes.util.QueryParser.parseQuery(v)).getOrElse(Map[Symbol, String]())

    HttpRequest[T](method, uri, allParameters, headers, content, remoteHost, version, subpath)
  }

  implicit def show[A]: Show[HttpRequest[A]] = new Show[HttpRequest[A]] {
    override def shows(r: HttpRequest[A]) = {
      JObject(
        "method" -> JString(r.method.toString),
        "uri" -> JString(r.uri.toString),
        "params" -> JObject(r.parameters map { case (k, v) => k.toString -> JString(v) } toMap),
        "headers" -> JString(r.headers.toString),
        "remoteHost" -> JString(r.remoteHost.toString),
        "subpath" -> JString(r.subpath)
      ).renderPretty
    }
  }
}
