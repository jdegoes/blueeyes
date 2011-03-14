package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.concurrent.Future
import blueeyes.util.PartialFunctionCombinators
import blueeyes.core.data._

trait HttpRequestHandlerImplicits extends PartialFunctionCombinators {
  implicit def identifierToIdentifierWithDefault[S](default: => S) = new {
    def ?: [T](identifier: T) = {
      IdentifierWithDefault[T, S](identifier, () => default)
    }
  }
  
  implicit def identifierToIdentifierWithErrorDefault[T <: AnyRef, S](identifier: T) = IdentifierWithDefault[T, S](identifier, () => error("Expected to find value for " + identifier.toString))
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits