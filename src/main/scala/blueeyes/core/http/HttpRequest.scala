package blueeyes.core.http

import blueeyes.core.http.HttpVersions._
import HttpHeaders._

import java.net.InetAddress

sealed case class HttpRequest[T] private(method: HttpMethod, uri: URI, parameters: Map[Symbol, String], headers: HttpHeaders, content: Option[T], remoteHost: Option[InetAddress], version: HttpVersion, subpath: String) {

  lazy val mimeTypes: List[MimeType] = (for (`Content-Type`(mimeTypes) <- headers.raw) yield mimeTypes.toList).toList.flatten

  def withSubpath(p: String) = copy(subpath = p)

  def withUriChanges(scheme: Option[String] = this.uri.scheme, userInfo: Option[String] = this.uri.userInfo, host: Option[String] = this.uri.host, port: Option[Int] = this.uri.port, path: Option[String] = this.uri.path, query: Option[String] = this.uri.query, fragment: Option[String] = this.uri.fragment) = {
    val newUri = URI(scheme, userInfo, host, port, path, query, fragment)

    copy(uri = newUri)
  }
}

object HttpRequest{
  def apply[T](method: HttpMethod, uri: URI, parameters: Map[Symbol, String] = Map(), headers: HttpHeaders = HttpHeaders.Empty, content: Option[T] = None, remoteHost: Option[InetAddress] = None, version: HttpVersion = `HTTP/1.1`): HttpRequest[T] = {
    val subpath   = uri.path.getOrElse("")
    val query     = uri.query
    val allParameters = parameters ++ query.map(v => blueeyes.util.QueryParser.parseQuery(v)).getOrElse(Map[Symbol, String]())

    HttpRequest[T](method, uri, allParameters, headers, content, remoteHost, version, subpath)
  }
}
