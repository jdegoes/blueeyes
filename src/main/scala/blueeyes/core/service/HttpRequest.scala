package blueeyes.core.service

import blueeyes.core.http.{HttpMethod, HttpVersion}
import blueeyes.core.http.HttpVersions._


//import HttpVersions._

sealed case class HttpRequest[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`)
