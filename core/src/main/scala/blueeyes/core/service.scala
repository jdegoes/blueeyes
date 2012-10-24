package blueeyes.core

import akka.dispatch.Future
import blueeyes.core.http._

package object service {
  type AsyncHttpService[T]       = HttpService[T, Future[HttpResponse[T]]]

  type HttpClientHandler[T]      = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

  type HttpServiceHandler[T, S]  = HttpRequest[T] => S

  type HttpClientTransformer[T, S] = HttpClient[T] => Future[S]

  type ServiceDescriptorFactory[T, S] = ServiceContext => ServiceLifecycle[T, S]

  type HttpResponseTransformer[T, S] = HttpResponse[T] => Future[S]

  type HttpServiceCombinator[A, B, C, D] = HttpService[A, B] => HttpService[C, D]
}
