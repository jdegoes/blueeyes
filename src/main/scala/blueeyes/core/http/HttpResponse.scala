package blueeyes.core.http

import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpVersions._

sealed case class HttpResponse[T](status: HttpStatus = HttpStatus(OK), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = `HTTP/1.1`)
