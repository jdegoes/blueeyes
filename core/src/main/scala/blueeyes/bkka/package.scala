package blueeyes

import akka.dispatch.Future
import akka.dispatch.MessageDispatcher

package object bkka {
  case class ~[A, B](a: A, b: B)

  class RichFuture[A](future: Future[A]) {
    def ~[B](other: Future[B])(implicit dispatcher: MessageDispatcher): Future[A ~ B] = {
      Future.sequence(future :: other :: Nil) map {
        case x :: y :: Nil => new ~(x.asInstanceOf[A], y.asInstanceOf[B])
      }
    }
  }

  implicit def f2rf[A](f: Future[A]): RichFuture[A] = new RichFuture(f)
}

// vim: set ts=4 sw=4 et:
