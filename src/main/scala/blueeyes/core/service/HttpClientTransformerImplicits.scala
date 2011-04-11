package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import java.util.concurrent.Future

/*trait HttpClientTransformerImplicits{
  implicit def httpClientCompositionSugar[T](h1: HttpClient[T]) = new {
    /** Joins two clients together. They are executed concurrently,
     * not sequentially.
     */
    def ~ [V](h2: HttpClient[V]) = (client: HttpClient[T]) => new HttpClient[(T, V)] {
      def isDefinedAt(request: HttpRequest[(T, V)]):  )

      def apply(request: HttpRequest[A]): Future[HttpResponse[A]] = self.apply {
        copy(request)
      }
    }

      //h1(client).zip(h2(client))
    
    /** Joins two clients together. They are executed sequentially,
     * not concurrently.
     */
    def ~> [V](h2: HttpClient[T, V]) = (client: HttpClient[T]) => h1(client).flatMap { r1 =>
      h2(client).map { r2 =>
        (r1, r2)
      }
    }
  }
}
object HttpClientTransformerImplicits extends HttpClientTransformerImplicits*/