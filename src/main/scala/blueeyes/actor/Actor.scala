package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

/** A purely functional actor, based on mealy machines. To achieve a useful 
 * level of functionality, you should import Actor._ or ActorM._,
 * depending on whether or not you are working with monadic output values.
 *
 * This trait is invariant because some actors may have complex dependencies 
 * between input / output types and some other polymorphic type.
 */
trait ActorV[A, B] extends (A => ActorState[A, B]) with NewType[A => ActorState[A, B]] with Serializable { self =>
  final val value: A => ActorState[A, B] = (a: A) => receive(a)

  final def apply(a: A): ActorState[A, B] = receive(a)

  final def ! (a: A): ActorState[A, B] = receive(a)

  /** Send multiple values and collect all the results.
   */
  final def !! (head: A, tail: A*): (Seq[B], Actor[A, B]) = {
    val (b, actor) = self ! head

    tail.foldLeft((Vector(b), actor)) {
      case ((seq, actor), a) =>
        val (b, next) = actor ! a

        (seq :+ b, next)
    }
  }

  final def !! (as: Seq[A]): (Seq[B], Actor[A, B]) = !! (as(0), as.tail: _*)

  /** Send multiple values and combine the results with a semigroup.
   */
  final def !+! (head: A, tail: A*)(implicit semigroup: Semigroup[B]): ActorState[A, B] = {
    val (b, actor) = self ! head

    tail.foldLeft[(B, Actor[A, B])]((b, actor)) {
      case ((b1, actor), a) =>
        val (b2, next) = actor ! a

        (b1 |+| b2, next)
    }
  }

  final def !+! (as: Seq[A])(implicit semigroup: Semigroup[B]): ActorState[A, B] = !+! (as(0), as.tail: _*)

  protected def receive(a: A): ActorState[A, B]

  override def equals(that: Any): Boolean = that match {
    case that: ActorV[_, _] => this.value == that.value

    case _ => false
  }

  override def hashCode: Int = value.hashCode
}

object Actor  extends ActorModule
object ActorM extends ActorMModule

/**
 * Alternate way to factor Actor. Allows preservation of "self-type" but the 
 * value of that is limited because most actors are composed.
 */
// trait Actor0[A, B, Z <: Actor0[A, B, Z]] extends (A => (B, Z)) with NewType[A => (B, Z)] 