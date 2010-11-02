package blueeyes.core.service

import blueeyes.core.http.{HttpMethod, HttpVersion}
import blueeyes.core.http.HttpVersions._

//import HttpVersions._
sealed case class HttpRequest[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, remoteHost: Option[String] = None, version: HttpVersion = `HTTP/1.1`) {
  import java.net.URI
  
  def path = new URI(uri).getPath
  
  def port = new URI(uri).getPort
  
  def query = new URI(uri).getQuery
  
  def fragment = new URI(uri).getFragment
  
  def authority = new URI(uri).getAuthority
  
  def scheme = new URI(uri).getScheme
  
  def schemeSpecificPart = new URI(uri).getSchemeSpecificPart
  
  def userInfo = new URI(uri).getUserInfo
  
  def isUriAbsolute = new URI(uri).isAbsolute
  
  def isUriOpaque = new URI(uri).isOpaque
}
