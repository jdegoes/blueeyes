package blueeyes.concurrent

import org.spex.Specification
import org.specs.util.TimeConversions._
import java.util.concurrent.CountDownLatch

class ActorSpec extends Specification with ActorExecutionStrategySequential {

  private val actorImplementation = new ActorImplementationSequential{}

//  "Actor" should {
//    "process message" in {
//      import actorImplementation._
//      val messageProcessor = actor[String, String] {
//        case message: String => message + "_done"
//      }
//
//     messageProcessor("foo").value must eventually (beSome("foo_done"))
//    }
//  }

//  "Actor of future" should {
//    "flatten implicitly" in {
//      import actorImplementation._
//
//      val actor1 = actor[String, String] {
//        case message: String => message + "_done"
//      }
//
//      val actor2: Actor[String, String] = Actor.actor[String, Future[String]] {
//        case message: String  => actor1 ! message
//      }
//
//      (actor2 ! "foo").value must eventually (beSome("foo_done"))
//    }
//  }

  "Actor examples" should {
    "compile" in {
      import Actor._
      // Easy actor definition:
      val squarePositiveA = actor[Int, Int] {
        case x: Int if (x > 0) => x * x
      }

      val squareNegativeA = actor[Int, Int] {
        case x: Int if (x < 0) => x * x
      }

      // Constant actors:
      val zeroA = Actor.constant[Int, Int](0)

      // Actor composition based on partial functions:
      val squareAnythingA = squarePositiveA.orElse(squareNegativeA).orElse(zeroA)

      // Actor composition based on arrows:
      val pow4A = squareAnythingA >>> squareAnythingA

      // Actor response mapping:
      val pow4AsStringA = pow4A.map(_.toString)

      // Safe actor-to-actor communication:
      val doublePow4A: Actor[Int, Int] = actor[Int, Future[Int]] {
       case x: Int => for (xTo4 <- (pow4A ! x)) yield xTo4 * 2
      }

      (pow4AsStringA ! 0).value must eventually (beSome("0"))
      (pow4AsStringA ! 1).value must eventually (beSome("1"))
      (pow4AsStringA ! 2).value must eventually (beSome("16"))
      awaitFuture((doublePow4A   ! 2)) must beSome(32)
    }
  }

  private def awaitFuture(future: Future[_]) = {
    val countDownLatch = new CountDownLatch(1)
    future deliverTo { v =>
      countDownLatch.countDown
    }
    countDownLatch.await

    future.value
   }


//  "Actor to Actor" in{
//    "deliver future in actor thread" in{
//      import Actor._
//      var deliveried = false
//      val rootActor = actor[String, Unit]{
//        case message: String => {
//          val thread  = Thread.currentThread
//           val nestedActor = actor[String, String]{
//            case message: String => "answer"
//           }
//          val future = nestedActor("bar")
//
//          future.deliverTo{v =>
//            deliveried = thread == Thread.currentThread
//          }
//        }
//      }
//      rootActor("test")
//
//      deliveried must eventually (be(true))
//    }
//  }
}