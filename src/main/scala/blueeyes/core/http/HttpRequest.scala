package blueeyes.core.http

import blueeyes.core.http.HttpVersions._
import java.net.InetAddress
import java.net.URI

//import HttpVersions._
sealed case class HttpRequest[T] private(method: HttpMethod, uri: String, parameters: Map[Symbol, String], headers: Map[String, String], content: Option[T], remoteHost: Option[InetAddress], version: HttpVersion, subpath: String) {

  def path = new URI(uri).getPath
  
  def host = new URI(uri).getHost
  
  def port = new URI(uri).getPort
  
  def query = new URI(uri).getQuery
  
  def fragment = new URI(uri).getFragment
  
  def authority = new URI(uri).getAuthority
  
  def scheme = new URI(uri).getScheme
  
  def schemeSpecificPart = new URI(uri).getSchemeSpecificPart
  
  def userInfo = new URI(uri).getUserInfo
  
  def isUriAbsolute = new URI(uri).isAbsolute
  
  def isUriOpaque = new URI(uri).isOpaque
  
  def withSubpath(p: String) = copy(subpath = p)
  
  def withUriChanges(scheme: String = this.scheme, userInfo: String = this.userInfo, host: String = this.host, port: Int = this.port, path: String = this.path, query: String = this.query, fragment: String = this.fragment) = {
    val newUri = new URI(scheme, userInfo, host, port, path, query, fragment)
    
    copy(uri = newUri.toString)
  }
}

object HttpRequest{
  def apply[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[InetAddress] = None, version: HttpVersion = `HTTP/1.1`): HttpRequest[T] = {
    val subpath = new URI(uri).getPath

    HttpRequest[T](method, uri, parameters, headers, content, remoteHost, version, subpath)
  }
}
