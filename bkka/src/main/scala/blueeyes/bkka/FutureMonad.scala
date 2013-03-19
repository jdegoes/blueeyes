package blueeyes.bkka

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.dispatch.Await

import scalaz._

class FutureMonad(context: ExecutionContext) extends Applicative[Future] with Monad[Future] {
  def point[A](a: => A) = Future(a)(context)
  def bind[A, B](fut: Future[A])(f: A => Future[B]) = fut.flatMap(f)
  override def ap[A,B](fa: => Future[A])(ff: => Future[A => B]) = (fa zip ff) map { case (a, f) => f(a) }
}

class UnsafeFutureComonad(context: ExecutionContext, copointMaxWait: akka.util.Duration) extends FutureMonad(context) with Comonad[Future] {
  def copoint[A](m: Future[A]) = Await.result(m, copointMaxWait)
  def cojoin[A](a: Future[A]): Future[Future[A]] = Promise.successful(a)(context)
  def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Promise.successful(f(fa))(context)
}

object FutureMonad {
  def M(implicit context: ExecutionContext): Monad[Future] = new FutureMonad(context)
}
