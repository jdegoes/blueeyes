package blueeyes.core.http

import blueeyes.core.http.HttpVersions._
import java.net.InetAddress
import java.net.URI

//import HttpVersions._
sealed case class HttpRequest[T] private(method: HttpMethod, uri: String, parameters: Map[Symbol, String], headers: HttpHeaders, content: Option[T], remoteHost: Option[InetAddress], version: HttpVersion, subpath: String) {

  def path                = nonNull(new URI(uri).getPath)
  def host                = nonNull(new URI(uri).getHost)
  def port                = new URI(uri).getPort
  def query               = nonNull(new URI(uri).getQuery)
  def fragment            = nonNull(new URI(uri).getFragment)
  def authority           = nonNull(new URI(uri).getAuthority)
  def scheme              = nonNull(new URI(uri).getScheme)
  def schemeSpecificPart  = nonNull(new URI(uri).getSchemeSpecificPart)
  def userInfo            = nonNull(new URI(uri).getUserInfo)
  def isUriAbsolute       = new URI(uri).isAbsolute
  def isUriOpaque         = new URI(uri).isOpaque

  def withSubpath(p: String) = copy(subpath = p)

  def withUriChanges(scheme: String = this.scheme, userInfo: String = this.userInfo, host: String = this.host, port: Int = this.port, path: String = this.path, query: String = this.query, fragment: String = this.fragment) = {
    val newUri = new URI(scheme, userInfo, host, port, path, query, fragment)

    copy(uri = newUri.toString)
  }

  private def nonNull(s: String, default: String = ""): String = s match {
    case null => default
    case s: String => s
  }
}

object HttpRequest{
  def apply[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: HttpHeaders = HttpHeaders(), content: Option[T] = None, remoteHost: Option[InetAddress] = None, version: HttpVersion = `HTTP/1.1`): HttpRequest[T] = {
    val subpath = new URI(uri).getPath

    val query = uri.indexOf("?") match {
      case -1 => ""

      case idx => uri.substring(idx + 1)
    }

    HttpRequest[T](method, uri, parameters ++ blueeyes.util.QueryParser.parseQuery(query), headers, content, remoteHost, version, subpath)
  }
}
