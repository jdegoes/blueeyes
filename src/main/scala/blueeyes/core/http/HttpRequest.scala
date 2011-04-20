package blueeyes.core.http

import blueeyes.core.http.HttpVersions._
import java.net.InetAddress

//import HttpVersions._
sealed case class HttpRequest[T] private(method: HttpMethod, uri: String, parameters: Map[Symbol, String], headers: HttpHeaders, content: Option[T], remoteHost: Option[InetAddress], version: HttpVersion, subpath: String) {

  private lazy val _uri = URI(uri) 

  def path                = _uri.path
  def host                = _uri.host
  def port                = _uri.port
  def query               = _uri.query
  def fragment            = _uri.fragment
  def authority           = _uri.authority
  def scheme              = _uri.scheme
  def userInfo            = _uri.userInfo
  def isUriAbsolute       = _uri.isAbsolute

  def withSubpath(p: String) = copy(subpath = p)

  def withUriChanges(scheme: Option[String] = this.scheme, userInfo: Option[String] = this.userInfo, host: Option[String] = this.host, port: Option[Int] = this.port, path: Option[String] = this.path, query: Option[String] = this.query, fragment: Option[String] = this.fragment) = {
    val newUri = URI(scheme, userInfo, host, port, path, query, fragment)

    copy(uri = newUri.toString)
  }
}

object HttpRequest{
  def apply[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: HttpHeaders = HttpHeaders(), content: Option[T] = None, remoteHost: Option[InetAddress] = None, version: HttpVersion = `HTTP/1.1`): HttpRequest[T] = {
    val parsedURI = URI(uri)
    val subpath   = parsedURI.path.getOrElse("")
    val query     = parsedURI.query
    val allParameters = parameters ++ query.map(v => blueeyes.util.QueryParser.parseQuery(v)).getOrElse(Map[Symbol, String]())

    HttpRequest[T](method, uri, allParameters, headers, content, remoteHost, version, subpath)
  }
}
