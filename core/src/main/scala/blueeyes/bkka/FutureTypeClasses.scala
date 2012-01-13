package blueeyes.bkka

import akka.dispatch.Future
import akka.dispatch.MessageDispatcher

import scalaz._

object AkkaTypeClasses {
  implicit def futureApplicative[A](implicit context: MessageDispatcher) = new Applicative[Future] {
    def point[A](a: => A) = Future(a)(context) 
    def ap[A,B](fa: => Future[A])(ff: => Future[A => B]) = fa.flatMap{ a => ff.map{ f => f(a) } }
  }
}
