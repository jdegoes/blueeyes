package blueeyes.core.service

trait HttpClientTransformerImplicits{
  implicit def httpResponseHandlerCompositionSugar[T, S](h1: HttpClientTransformer[T, S]) = new {
    def ~ [V](h2: HttpClientTransformer[T, V]) = {
      (client: HttpClient[T]) => {
        h1(client).flatMap(v1 => {
          h2(client).map(v2 => (v1, v2))
        })
      }
    }
  }
}
object HttpClientTransformerImplicits extends HttpClientTransformerImplicits
