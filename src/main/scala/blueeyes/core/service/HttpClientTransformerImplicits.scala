package blueeyes.core.service

trait HttpClientTransformerImplicits{
  implicit def httpResponseHandlerCompositionSugar[T, S](h1: HttpClientTransformer[T, S]) = new {
    def ~ [V](h2: HttpClientTransformer[T, V]) = (client: HttpClient[T]) => h1(client).zip(h2(client))
  }
}
object HttpClientTransformerImplicits extends HttpClientTransformerImplicits
