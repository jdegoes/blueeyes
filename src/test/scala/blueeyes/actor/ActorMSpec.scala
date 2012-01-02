package blueeyes.actor

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import scala.util.Random

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

import scalaz._
import scalaz.Scalaz._

class ActorMSpec extends Specification with ScalaCheck {
  import ActorTModule._

  val double: ActorT[Option, Int, Int] = moore((v: Int) => v * 2)

  val halve: ActorT[Option, Int, Int] = moore((v: Int) => v / 2)

  val doubleEveryOther: ActorT[Option, Int, Int] = {
    def d0(double: Boolean): ActorT[Option, Int, Int] = receive { a: Int =>
      Some((if (double) a * 2 else a, d0(!double)))
    }
    
    d0(true)
  }


  "Actor !" should {
    "return result" in {
      (double ! 2) must beLike {
        case Some((v, _)) => v mustEqual 4
      }
    }
  }

  "Actor !!" should {
    "return sequence of results for stateless actor in Option monad" in {
      (double !! (2, 4, 6)) must beLike {
        case Some((v, _)) => v.toList mustEqual (4 :: 8 :: 12 :: Nil)
      }
    }

    "return sequence of results for stateful actor in Option monad" in {
      (doubleEveryOther !! (2, 4, 6)) must beLike {
        case Some((v, _)) => v.toList mustEqual (4 :: 4 :: 12 :: Nil)
      }
    }
  }

  "Actor >>>" should {
    "compose actors in the option monad" in {
      ((double >>> halve) !! (1, 2, 3, 4)) must beLike {
        case Some((v, _)) => v.toList mustEqual (1 :: 2 :: 3 :: 4 :: Nil)
      }
    }
  }
}