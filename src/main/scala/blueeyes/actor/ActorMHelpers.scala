package blueeyes.actor

import scalaz._
import scalaz.Validation._

trait ActorMHelpers {
  def identityActor[M[_], A](implicit monad: Monad[M]) = moore(identity[A])(monad)
  
  def receive[M[_], A, B](fn: A => ActorMState[M, A, B]): ActorM[M, A, B] = new ActorM[M, A, B] {
    final def receive(a: A): ActorMState[M, A, B] = fn(a)
  }

  def receiveSome[M[_], A, B, E >: MatchError](fn: PartialFunction[A, ActorMState[M, A, Validation[E, B]]])
      (implicit monad: Monad[M]): ActorM[M, A, Validation[E, B]] = {
    def receiveSome0(fn: A => ActorMState[M, A, Validation[E, B]]): ActorM[M, A, Validation[E, B]] = {
      lazy val lazySelf: ActorM[M, A, Validation[E, B]] = receive[M, A, Validation[E, B]] { a: A =>
        try {
          fn(a)
        }
        catch {
          case e: MatchError => monad.point((failure(e), lazySelf))
        }
      }

      lazySelf
    }

    receiveSome0(fn)
  }

  def playback[M[_], A, B](i: Iterable[B])(implicit monad: Monad[M]): ActorM[M, A, B] = receive { a: A =>
    monad.point((i.head, playback(i.tail)))
  }

  def moore[M[_], A, B](f: A => B)(implicit monad: Monad[M]): ActorM[M, A, B] = {
    lazy val self: ActorM[M, A, B] = receive[M, A, B] { a: A =>
      monad.point((f(a), self))
    }

    self
  }

  def split[M[_], A](implicit monad: Monad[M]): ActorM[M, A, (A, A)] = {
    lazy val lazySelf: ActorM[M, A, (A, A)] = receive { a: A =>
      monad.point(((a, a), lazySelf))
    }

    lazySelf
  }
}
object ActorMHelpers extends ActorMHelpers
