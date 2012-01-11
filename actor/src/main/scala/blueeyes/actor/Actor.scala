package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

/** A purely functional actor, based on mealy machines.
 *
 * This trait is invariant because some actors may have complex dependencies 
 * between input / output types and some other polymorphic type.
 */
trait Actor[A, B] extends (A => ActorState[A, B]) with Serializable { self =>
  final def apply(a: A): ActorState[A, B] = receive(a)

  final def ! (a: A): ActorState[A, B] = receive(a)

  /** Send multiple values and collect all the results.
   */
  final def !! (head: A, tail: A*): (Seq[B], Actor[A, B]) = !! (head +: tail)

  final def !! (as: Seq[A]): (Seq[B], Actor[A, B]) = {
    if (as.length == 0) (Vector.empty, self)
    else {
      val head = as.head
      val tail = as.tail

      val (b, next1) = self ! head
      val (bs, next2) = next1 !! tail

      (b +: bs, next2)
    }
  }

  /** Send multiple values and combine the results with a semigroup.
   */
  final def !+! (head: A, tail: A*)(implicit monoid: Monoid[B]): ActorState[A, B] = !+! (head +: tail)

  final def !+! (as: Seq[A])(implicit monoid: Monoid[B]): ActorState[A, B] = {
    if (as.length == 0) (monoid.zero, self)
    else {
      val head = as.head
      val tail = as.tail

      val (b1, next1) = self ! head
      val (b2, next2) = next1 !+! tail

      (b1 |+| b2, next2)
    }
  }

  protected def receive(a: A): ActorState[A, B]
}

object Actor extends ActorModule
