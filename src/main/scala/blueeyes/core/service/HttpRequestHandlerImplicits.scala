package blueeyes.core.service

import blueeyes.util.PartialFunctionCombinators

trait HttpRequestHandlerImplicits extends PartialFunctionCombinators {
  implicit def identifierToIdentifierWithDefault[S](default: => S) = new ToIdentifierWithDefault(default)
  class ToIdentifierWithDefault[S](default: => S){
    def ?: [T](identifier: T) = IdentifierWithDefault[T, S](identifier, Some(default))
  }
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits
