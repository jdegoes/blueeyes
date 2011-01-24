package blueeyes.core.service

trait HttpClientTransformerImplicits{
  implicit def httpResponseHandlerCompositionSugar[T, S](h1: HttpClientTransformer[T, S]) = new {
    /** Joins two client transformers together. They are executed concurrently, 
     * not sequentially.
     */
    def ~ [V](h2: HttpClientTransformer[T, V]) = (client: HttpClient[T]) => h1(client).zip(h2(client))
    
    /** Joins two client transformers together. They are executed sequentially, 
     * not concurrently.
     */
    def ~> [V](h2: HttpClientTransformer[T, V]) = (client: HttpClient[T]) => h1(client).flatMap { r1 =>
      h2(client).map { r2 =>
        (r1, r2)
      }
    }
  }
}
object HttpClientTransformerImplicits extends HttpClientTransformerImplicits