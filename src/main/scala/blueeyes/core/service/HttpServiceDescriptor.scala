package blueeyes.core.service

import blueeyes.util.Future

case class HttpServiceDescriptor[T, S](startup: () => Future[S], request: S => HttpRequestHandler[T], shutdown: S => Future[Unit]) { self =>
  def ~ [R](that: HttpServiceDescriptor[T, R]): HttpServiceDescriptor[T, (S, R)] = {
    HttpServiceDescriptor[T, (S, R)](
      startup  = () => self.startup().zip(that.startup()),
      request  = (pair: (S, R)) => self.request(pair._1).orElse(that.request(pair._2)),
      shutdown = (pair: (S, R)) => self.shutdown(pair._1).zip(that.shutdown(pair._2)).map(_ => Unit)
    )
  }
  
  def ~ (that: HttpRequestHandler[T]): HttpServiceDescriptor[T, S] = {
    HttpServiceDescriptor[T, S](
      startup  = self.startup,
      request  = (s: S) => {
        self.request(s).orElse(that)
      },
      shutdown = self.shutdown
    )
  }
}