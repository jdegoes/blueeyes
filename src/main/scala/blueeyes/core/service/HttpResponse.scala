package blueeyes.core.service

import HttpVersions._
import HttpStatusCodes._

sealed case class HttpResponse[T](status: HttpStatus = HttpStatus(OK), headers: Map[String, String] = Map(), content: Option[T] = None, version: HttpVersion = Http_1_1)