package blueeyes.core

import blueeyes.core.http._
import blueeyes.core.data._

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import scalaz._
import scalaz.syntax.traverse._
import scalaz.std.option._

package object service {
  type AsyncHttpService[T]       = HttpService[T, Future[HttpResponse[T]]]
  type AsyncHttpTranscoder[A, B] = AsyncTranscoder[HttpRequest, HttpResponse, A, B]

  type HttpClientHandler[T]      = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

  type HttpServiceHandler[T, S]  = HttpRequest[T] => S

  type HttpClientTransformer[T, S] = HttpClient[T] => Future[S]

  type ServiceDescriptorFactory[T, S] = ServiceContext => ServiceLifecycle[T, S]

  type HttpResponseTransformer[T, S] = HttpResponse[T] => Future[S]

  type HttpServiceCombinator[A, B, C, D] = HttpService[A, B] => HttpService[C, D]

  implicit def asyncHttpTranscoder[A, B](implicit M: Monad[Future], projection: A => B, surjection: B => Future[A]): AsyncHttpTranscoder[A, B] = {
    new AsyncTranscoder[HttpRequest, HttpResponse, A, B] {
      def apply(request: HttpRequest[A]) = request.copy(content = request.content.map(projection))
      def unapply(responseFuture: Future[HttpResponse[B]]) = {
        for {
          response <- responseFuture
          content  <- response.content.map(surjection).sequence
        } yield {
          response.copy(content = content)
        }
      }
    }
  }

  implicit def identityHttpTranscoder[A]: AsyncHttpTranscoder[A, A] = {
    new AsyncTranscoder[HttpRequest, HttpResponse, A, A] {
      def apply(request: HttpRequest[A]) = request
      def unapply(responseFuture: Future[HttpResponse[A]]) = responseFuture
    }
  }

  implicit def unpackFutureContent[A, B](responseFuture: Future[HttpResponse[A]])(implicit M: Monad[Future], f: A => Future[B]): Future[HttpResponse[B]] = {
    for {
      response <- responseFuture
      content  <- response.content.map(f).sequence
    } yield {
      response.copy(content = content)
    }
  }
}
