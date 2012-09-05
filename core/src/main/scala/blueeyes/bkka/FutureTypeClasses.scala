package blueeyes.bkka

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import scalaz._

class FutureMonad(implicit context: ExecutionContext) extends Applicative[Future] with Monad[Future] {
  def point[A](a: => A) = Future(a)(context) 
  def bind[A, B](fut: Future[A])(f: A => Future[B]) = fut.flatMap(f)
  override def ap[A,B](fa: => Future[A])(ff: => Future[A => B]) = fa.flatMap{ a => ff.map{ f => f(a) } }
}

object AkkaTypeClasses {
  implicit def futureApplicative(implicit context: ExecutionContext) = new FutureMonad
}
