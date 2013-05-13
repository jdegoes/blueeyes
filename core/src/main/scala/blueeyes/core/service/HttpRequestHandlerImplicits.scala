package blueeyes.core.service

import blueeyes.core.http._
import blueeyes.util.PartialFunctionCombinators

import scalaz._
import scalaz.syntax.functor._

trait HttpRequestHandlerImplicits extends PartialFunctionCombinators {
  implicit def identifierToIdentifierWithDefault[S](default: => S) = new ToIdentifierWithDefault(default)

  class ToIdentifierWithDefault[S](default: => S){
    def ?: [T](identifier: T) = IdentifierWithDefault[T, S](identifier, Some(default))
  }

  implicit def liftToResponse[A, B, F[_]](resp: F[HttpResponse[A]])(implicit f: A => B, F: Functor[F]): F[HttpResponse[B]] = {
    resp map { _ map f }
  }
}

object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits
