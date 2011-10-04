package blueeyes.core.service

import blueeyes.core.data.Unapply

trait HttpServicePimps {

  implicit def ToComposableHttpService[A, B](service: HttpServices.HttpService[A, B]) = new ComposableHttpService(service)

  case class ComposableHttpService[A, B](service: HttpServices.HttpService[A, B]){
    def ~ (other: HttpServices.HttpService[A, B]) = HttpServices.OrService(service, other)
    def ~ [C, D](other: HttpServices.HttpService[C, D])(implicit unapply: Unapply[C, A], apply: D => B) = HttpServices.OrService(service, other.contramap(unapply.unapply).map(apply))
  }
}

object HttpServicePimps extends HttpServicePimps