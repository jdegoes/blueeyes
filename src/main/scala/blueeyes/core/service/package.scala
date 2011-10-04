package blueeyes.core

import blueeyes.concurrent.Future
import blueeyes.core.http._

package object service{
  type AsyncHttpService[T]       = HttpServices.HttpService[T, Future[HttpResponse[T]]]
  type AsyncCustomHttpService[T] = HttpServices.CustomHttpService[T, Future[HttpResponse[T]]]

  type HttpClientHandler[T]      = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]

  type HttpServiceHandler[T, S]  = HttpRequest[T] => S

  type HttpClientTransformer[T, S] = HttpClient[T] => Future[S]

  type HttpServiceDescriptorFactory[T, S] = HttpServiceContext => HttpServiceDescriptor[T, S]

  type HttpResponseTransformer[T, S] = HttpResponse[T] => Future[S]
}
