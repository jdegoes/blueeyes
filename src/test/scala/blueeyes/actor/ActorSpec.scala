package blueeyes.actor

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import scala.util.Random

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

import scalaz._
import scalaz.Scalaz._

class ActorSpec extends Specification with ScalaCheck {
  import ActorModule._

  val double: Actor[Int, Int] = moore(_ * 2)

  val triple: Actor[Int, Int] = moore(_ * 3)

  val divideOpt: Actor[(Int, Int), Option[Float]] = moore {
    case ((v1: Int, v2: Int)) => if (v2 == 0) None else Some[Float](v1 / v2)
  }

  "Actor !" should {
    "return result" in {
      (double ! 2)._1 mustEqual 4
    }
  }

  "map" should {
    "map output values" in {
      (double.map(_ / 2) ! 3)._1 mustEqual 3
    }
  }

  "premap" should {
    "map input values" in {
      val doubleS = double.premap((s: String) => s.toInt)

      (doubleS ! "5")._1 mustEqual 10
    }
  }

  "&" should {
    "return both output values" in {
      ((double & triple) ! 2)._1 mustEqual ((4, 6))
    }
  }

  ">-" should {
    "merge two output values" in {
      (((double & triple) >- (_ + _)) ! 2)._1 mustEqual 10
    }
  }

  "filter" should {
    "remove rejected output values" in {
      val filtered = divideOpt.filter(_.map(_ < 5).getOrElse(false))

      (filtered ! ((10, 1)))._1 mustEqual(None)
      (filtered ! ((4, 1)))._1 mustEqual(Some(4.0F))
    }
  }

  ">>>" should {
    "compose actors" in {
      ((double >>> triple) ! 2)._1 mustEqual (12)
    }
  }

  "*" should {
    "cross actors" in {
      ((double * triple) ! (2, 1))._1 mustEqual ((4, 3))
    }
  }

  "switch" should {
    "switch between actors based on boolean predicates" in {
      val a = identityActor[Int].switch(identityActor[Int])(
        ((v: Int) => v > 2 && v <= 4) -> double,
        ((v: Int) => v > 4)           -> triple
      )

      (a ! -123)._1 mustEqual(-123)
      (a ! 3)._1 mustEqual(6)
      (a ! 5)._1 mustEqual(15)
    }
  }

  "scan" should {
    "scan over output values" in {
      val a = identityActor[Int].scan[List[Int]](Nil) {
        case (list, value) => value :: list
      }

      (a !! (1, 2, 3))._1 mustEqual (Vector(List(1), List(2, 1), List(3, 2, 1)))
    }
  }

  "fold" should {
    "fold over output values" in {
      (identityActor[Int].fold(0, List(1, 2, 3)) {
        case (sum, value) => sum + value
      })._1 mustEqual (6)
    }
  }
}