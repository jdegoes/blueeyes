package blueeyes.bkka

import akka.dispatch.Future
import scalaz.Monad

trait TestAkkaDefaults extends AkkaDefaults {
  implicit def M: Monad[Future] = new FutureMonad(defaultFutureDispatch)
}


// vim: set ts=4 sw=4 et:
