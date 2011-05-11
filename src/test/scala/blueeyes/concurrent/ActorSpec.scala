package blueeyes.concurrent

import org.specs.Specification
import org.specs.util.TimeConversions._

class ActorSpec extends Specification with ActorImplicits{

  "Actor" should {
    "process message" in {
      import ActorStrategySequential._
      val actor = new Actor{
        val messageProcessor = lift1{ message: String => message + "_done"}
      }

      actor.messageProcessor("foo").value must eventually (beSome("foo_done"))
    }
  }

  "Actor of future" should {
    "flatten implicitly" in {
      import ActorStrategySequential._
      val actor1 = new Actor{
        val messageProcessor = lift1{ message: String => message + "_done"}
      }

      val actor2 = new Actor{
        val messageProcessor: (String) => Future[String] = flatLift1{ message: String => actor1.messageProcessor(message)}
      }

      val future = actor2.messageProcessor("foo")
      future.value must eventually (beSome("foo_done"))
    }
  }

  "Actor examples" should {
    "compile" in {
      import ActorStrategy._
      // Easy actor definition:
      val actor = new Actor{
        val squarePositiveA = lift1 { x: Int => x * x }

        val squareNegativeA = lift1 { x: Int => x * x }

        val zeroA = constant[Int, Int](0)

        // Actor composition based on arrows:
        val pow4A: Int => Future[Int] = squarePositiveA >>> squareNegativeA

        // Actor response mapping:
        val pow4AsStringA = pow4A.map(_.toString)

        // Safe actor-to-actor communication:
        val doublePow4A: (Int) => Future[Int]  = flatLift1{ x: Int =>
          for (xTo4 <- (pow4A(x))) yield xTo4 * 2
        }
      }

      import actor._

      val pow4AsStringAFuture0 = (pow4AsStringA(0))
      pow4AsStringAFuture0.value must eventually (beSome("0"))
      val pow4AsStringAFuture1 = (pow4AsStringA(1))
      pow4AsStringAFuture1.value must eventually (beSome("1"))
      val pow4AsStringAFuture2 = (pow4AsStringA(2))
      pow4AsStringAFuture2.value must eventually (beSome("16"))
      val doublePow4AFuture = doublePow4A(2)
      doublePow4AFuture.value must eventually (beSome(32))
    }
  }
}