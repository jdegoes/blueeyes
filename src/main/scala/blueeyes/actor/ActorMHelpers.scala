package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorMHelpers {
  def receive[M[_], A, B](fn: A => ActorMState[M, A, B]): ActorM[M, A, B] = new Actor[A, M[B]] {
    final def receive(a: A) = fn(a)
  }

  def receiveSome[M[_], A, B, E >: MatchError](fn: PartialFunction[A, ActorMState[M, A, Validation[E, B]]])(pure: Pure[M]): ActorM[M, A, Validation[E, B]] = {
    def receiveSome0(fn: A => ActorMState[M, A, Validation[E, B]]): ActorM[M, A, Validation[E, B]] = {
      lazy val lazySelf: ActorM[M, A, Validation[E, B]] = receive[M, A, Validation[E, B]] { a: A =>
        try {
          fn(a)
        }
        catch {
          case e: MatchError => (pure.pure(failure(e)), lazySelf)
        }
      }

      lazySelf
    }

    receiveSome0(fn)
  }

  def playback[M[_], A, B](i: Iterable[B])(implicit pure: Pure[M]): ActorM[M, A, B] = receive { a: A =>
    (pure.pure(i.head), playback(i.tail))
  }

  def moore[M[_], A, B](f: A => B)(implicit monad: Monad[M]): ActorM[M, A, B] = {
    lazy val lazySelf: ActorM[M, A, B] = receive[M, A, B] { a: A =>
      (monad.pure(f(a)), lazySelf)
    }

    lazySelf
  }

  def reply[M[_], A, B](b: B)(implicit pure: Pure[M]) = (fn: A => ActorMState[M, A, B]) => (pure.pure(b), receive(fn))

  def reply[M[_], A, B](b: B, fn: A => ActorMState[M, A, B])(implicit pure: Pure[M]): ActorMState[M, A, B] = (pure.pure(b), receive(fn))

  def split[M[_], A](implicit pure: Pure[M]): ActorM[M, A, (A, A)] = {
    lazy val lazySelf: ActorM[M, A, (A, A)] = receive { a: A =>
      (pure.pure(a, a), lazySelf)
    }

    lazySelf
  }
}
object ActorMHelpers extends ActorMHelpers