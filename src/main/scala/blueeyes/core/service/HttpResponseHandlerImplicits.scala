package blueeyes.core.service

trait HttpResponseHandlerImplicits{
  def httpResponseHandlerCompositionSugar[T, S](h1: HttpClientHandler[T, S]) = new {
    def ~ [V](h2: HttpClientHandler[T, V]) = {
      (client: HttpClient[T]) => {
        h1(client).flatMap(v1 => {
          h2(client).map(v2 => (v1, v2))
        })
      }
    }
  }
}
object HttpResponseHandlerImplicits extends HttpResponseHandlerImplicits
