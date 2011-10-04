package blueeyes.core.service

import HttpServicePimps._
import blueeyes.concurrent.Future

case class HttpServiceDescriptor[T, S](startup: () => Future[S], request: S => AsyncHttpService[T], shutdown: S => Future[Unit]) { self =>
  def ~ [R](that: HttpServiceDescriptor[T, R]): HttpServiceDescriptor[T, (S, R)] = {
    HttpServiceDescriptor[T, (S, R)](
      startup  = () => self.startup().zip(that.startup()),
      request  = (pair: (S, R)) => self.request(pair._1) ~ (that.request(pair._2)),
      shutdown = (pair: (S, R)) => self.shutdown(pair._1).zip(that.shutdown(pair._2)).map(_ => Unit)
    )
  }

  def ~> (that: AsyncHttpService[T]): HttpServiceDescriptor[T, S] = {
    HttpServiceDescriptor[T, S](
      startup  = self.startup,
      request  = (s: S) => {
        that ~ self.request(s)
      },
      shutdown = self.shutdown
    )
  }
  
  def ~ (that: AsyncHttpService[T]): HttpServiceDescriptor[T, S] = {
    HttpServiceDescriptor[T, S](
      startup  = self.startup,
      request  = (s: S) => {
        self.request(s) ~ that
      },
      shutdown = self.shutdown
    )
  }
}