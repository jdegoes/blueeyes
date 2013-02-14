package blueeyes.bkka

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import scalaz._

class FutureMonad(context: ExecutionContext) extends Applicative[Future] with Monad[Future] {
  def point[A](a: => A) = Future(a)(context)
  def bind[A, B](fut: Future[A])(f: A => Future[B]) = fut.flatMap(f)
  override def ap[A,B](fa: => Future[A])(ff: => Future[A => B]) = (fa zip ff) map { case (a, f) => f(a) }
}

object FutureMonad {
  def M(implicit context: ExecutionContext): Monad[Future] = new FutureMonad(context)
}
