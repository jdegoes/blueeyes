package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorTFunctions {
  def identityActor[M[_], A](implicit monad: Monad[M]) = moore(identity[A])
  
  def receive[M[_], A, B](fn: A => ActorTState[M, A, B]): ActorT[M, A, B] = new ActorT[M, A, B] {
    final def receive(a: A): ActorTState[M, A, B] = fn(a)
  }

  def receiveOnly[M[_], A, B](fn: PartialFunction[A, ActorTState[M, A, B]])(implicit zero: Zero[B], monad: Monad[M]): ActorT[M, A, B] = new ActorT[M, A, B] {
    final def receive(a: A): ActorTState[M, A, B] = {
      if (fn.isDefinedAt(a)) fn(a)
      else monad.pure((zero.zero, receiveOnly(fn)))
    }
  }

  def receiveSome[M[_], A, B, E >: MatchError](fn: PartialFunction[A, ActorTState[M, A, Validation[E, B]]])
      (implicit monad: Monad[M]): ActorT[M, A, Validation[E, B]] = {
    def receiveSome0(fn: A => ActorTState[M, A, Validation[E, B]]): ActorT[M, A, Validation[E, B]] = {
      lazy val lazySelf: ActorT[M, A, Validation[E, B]] = receive[M, A, Validation[E, B]] { a: A =>
        try {
          fn(a)
        }
        catch {
          case e: MatchError => monad.pure((failure(e), lazySelf))
        }
      }

      lazySelf
    }

    receiveSome0(fn)
  }

  def playback[M[_], A, B](i: Iterable[B])(implicit monad: Monad[M]): ActorT[M, A, B] = receive { a: A =>
    monad.pure((i.head, playback(i.tail)))
  }

  def mooreM[M[_], A, B](f: A => M[B])(implicit monad: Monad[M]): ActorT[M, A, B] = {
    lazy val self: ActorT[M, A, B] = receive[M, A, B] { a: A =>
      f(a) map { b =>
        (b, self)
      }
    }

    self
  }

  def moore[M[_], A, B](f: A => B)(implicit monad: Monad[M]): ActorT[M, A, B] = {
    lazy val self: ActorT[M, A, B] = receive[M, A, B] { a: A =>
      monad.pure((f(a), self))
    }

    self
  }

  def split[M[_], A](implicit monad: Monad[M]): ActorT[M, A, (A, A)] = {
    lazy val lazySelf: ActorT[M, A, (A, A)] = receive { a: A =>
      monad.pure(((a, a), lazySelf))
    }

    lazySelf
  }
}
object ActorTFunctions extends ActorTFunctions