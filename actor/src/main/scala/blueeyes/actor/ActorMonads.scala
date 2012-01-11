package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

/** This is about the only useful monad you can derive without knowing more 
 * about the types. You would use this if you want to split orthogonal 
 * processing into separate actors and recombine the information through
 * monadic binding.
 */
trait ActorMonadParallel {
  implicit def ActorMonadParallel[A] = new Monad[({type λ[α]=Actor[A, α]})#λ] {
    def bind[B, C](self: Actor[A, B], f: B => Actor[A, C]): Actor[A, C] =  {
      ActorHelpers.receive { a: A =>
        val (b, next1) = self ! a

        val that = f(b)

        val (bb, next2) = that ! a

        (bb, bind(next1, (b: B) => next2))
      }
    }

    def pure[B](b: => B): Actor[A, B] = {
      lazy val lazySelf: Actor[A, B] = ActorHelpers.receive { a: A =>
        (b, lazySelf)
      }

      lazySelf
    }
  }
}
object ActorMonadParallel extends ActorMonadParallel
