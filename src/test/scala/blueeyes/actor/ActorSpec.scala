package blueeyes.actor

import org.specs.Specification
import org.specs.ScalaCheck

import scala.util.Random

import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

import scalaz._
import scalaz.Scalaz._

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Shrink

class ActorSpec extends Specification with ScalaCheck {
  import ActorModule._

  val double: Actor[Int, Int] = receive { i: Int =>
    reply(2 * i) {
      double
    }
  }

  val triple: Actor[Int, Int] = receive { i: Int =>
    reply(3 * i) {
      double
    }
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
}