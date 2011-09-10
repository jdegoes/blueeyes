package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorHelpers {
  def identityActor[A] = moore(identity[A])
  
  def receive[A, B](fn: A => ActorState[A, B]): Actor[A, B] = new Actor[A, B] {
    final def receive(a: A): ActorState[A, B] = fn(a)
  }

  def receiveSome[A, B, E >: MatchError](fn: PartialFunction[A, ActorState[A, Validation[E, B]]]): Actor[A, Validation[E, B]] = {
    def receiveSome0(fn: A => ActorState[A, Validation[E, B]]): Actor[A, Validation[E, B]] = {
      lazy val lazySelf: Actor[A, Validation[E, B]] = receive[A, Validation[E, B]] { a: A =>
        try {
          fn(a)
        }
        catch {
          case e: MatchError => (failure(e), lazySelf)
        }
      }

      lazySelf
    }

    receiveSome0(fn)
  }

  def playback[A, B](i: Iterable[B]): Actor[A, B] = receive { a: A =>
    (i.head, playback(i.tail))
  }

  def moore[A, B](f: A => B): Actor[A, B] = {
    lazy val self: Actor[A, B] = receive[A, B] { a: A =>
      (f(a), self)
    }

    self
  }

  def reply[A, B](b: B) = (fn: A => ActorState[A, B]) => (b, receive(fn))

  def reply[A, B](b: B, fn: A => ActorState[A, B]): ActorState[A, B] = (b, receive(fn))

  def split[A]: Actor[A, (A, A)] = {
    lazy val lazySelf: Actor[A, (A, A)] = receive { a: A =>
      ((a, a), lazySelf)
    }

    lazySelf
  }
}
object ActorHelpers extends ActorHelpers