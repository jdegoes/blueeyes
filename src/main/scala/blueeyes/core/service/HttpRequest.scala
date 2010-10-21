package blueeyes.core.service

import HttpVersions._

sealed case class HttpRequest[T](method: HttpMethod, uri: String, parameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`)
