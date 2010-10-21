package blueeyes.core.service

import blueeyes.core.http.{HttpVersions, HttpVersion}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpVersions._

sealed case class HttpResponse[T](status: HttpStatus = HttpStatus(OK), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`)
