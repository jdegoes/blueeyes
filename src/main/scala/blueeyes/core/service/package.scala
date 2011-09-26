package blueeyes.core

import blueeyes.concurrent.Future
import blueeyes.core.http._

package object service{
  type HttpRequestHandler[T]     = PartialFunction[HttpRequest[T], Future[HttpResponse[T]]]
  type HttpRequestHandler2[T, S] = PartialFunction[HttpRequest[T], Future[HttpResponse[S]]]
  
  type HttpRequestHandlerFull[T]     = HttpRequest[T] => Future[HttpResponse[T]]
  type HttpRequestHandlerFull2[T, S] = HttpRequest[T] => Future[HttpResponse[S]]
  type HttpRequestHandlerFull3[T, S] = HttpRequest[T] => S

  type HttpClientTransformer[T, S] = HttpClient[T] => Future[S]

  type HttpServiceDescriptorFactory[T, S] = HttpServiceContext => HttpServiceDescriptor[T, S]

  type HttpResponseTransformer[T, S] = HttpResponse[T] => Future[S]
}
