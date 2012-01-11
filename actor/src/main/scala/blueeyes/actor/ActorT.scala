package blueeyes.actor

import scalaz._
import Scalaz._

trait ActorT[M[_], A, B] extends (A => M[(B, ActorT[M, A, B])]) with Serializable { self =>
  final def apply(a: A): ActorTState[M, A, B] = receive(a)

  final def ! (a: A): ActorTState[M, A, B] = receive(a)

  /** Send multiple values and collect all the results.
   */
  final def !! (head: A, tail: A*)(implicit monad: Monad[M]): M[(Seq[B], ActorT[M, A, B])] = !! (head +: tail)

  final def !! (as: Seq[A])(implicit monad: Monad[M]): M[(Seq[B], ActorT[M, A, B])] = {
    if (as.length == 0) monad.pure((Vector.empty[B], self))
    else {
      val head = as.head
      val tail = as.tail

      (self ! head) flatMap {
        case (b, next1) =>
          (next1 !! tail) map { 
            case (bs, next2) =>
              (b +: bs, next2)
          }
      }
    }
  }

  /** Send multiple values and combine the results with a monoid.
   */
  final def !+! (head: A, tail: A*)(implicit monad: Monad[M], monoid: Monoid[B]): ActorTState[M, A, B] = !+! (head +: tail)

  final def !+! (as: Seq[A])(implicit monad: Monad[M], monoid: Monoid[B]): ActorTState[M, A, B] = {
    if (as.length == 0) monad.pure((monoid.zero, self))
    else {
      val head = as.head
      val tail = as.tail

      (self ! head) flatMap {
        case (b1, next1) =>
          (next1 !+! tail) map {
            case (b2, next2) =>
              (b1 |+| b2, next2)
          }
      }
    }
  }

  protected def receive(a: A): ActorTState[M, A, B]
}

object ActorT extends ActorTModule