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
trait ActorV[A, B] extends (A => ActorState[A, B]) with NewType[A => ActorState[A, B]] { self =>
  final val value: A => ActorState[A, B] = (a: A) => receive(a)

  final def apply(a: A): ActorState[A, B] = receive(a)

  final def ! (a: A): ActorState[A, B] = receive(a)

  final def !! (as: Seq[A]): (Seq[B], Actor[A, B]) = {
    as.foldLeft[(Vector[B], Actor[A, B])]((Vector.empty[B], self)) {
      case ((bs, actor), a) =>
        val (b, next) = actor ! a

        (bs :+ b, next)
        
    }
  }

  protected def receive(a: A): ActorState[A, B]
}

object Actor  extends ActorModule
object ActorM extends ActorMModule

/**
 * Alternate way to factor Actor. Allows preservation of "self-type" but the 
 * value of that is limited because most actors are composed.
 */
// trait Actor0[A, B, Z <: Actor0[A, B, Z]] extends (A => (B, Z)) with NewType[A => (B, Z)] 