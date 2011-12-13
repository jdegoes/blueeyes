package blueeyes.persistence.cache

import org.specs2.mutable.Specification
import org.specs2.time.TimeConversions._
import java.util.concurrent.TimeUnit.{MILLISECONDS}


import scala.util.Random
import scalaz._

import blueeyes.bkka.AkkaDefaults
import akka.dispatch.Future
import akka.dispatch.Future._
import akka.actor.Actor
import akka.actor.Props
import akka.util.Duration
import akka.util.Timeout

import java.util.concurrent.TimeUnit

class StageSpec extends Specification with AkkaDefaults {
  private val random    = new Random()
  implicit val timeout = Timeout(100000)
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + s2
  }

  "Stage" should {
    "evict when entry is expired" in {
      @volatile var evicted = false

      val stage = newStage(None, Some(20), (key: String, value: String) => evicted = evicted || (key == "foo" && value == "bar"))

      stage.put("foo", "bar")
      stage.put("bar", "baz")

      (evicted must eventually (beTrue))

      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    "evict when stage is over capacity" in{
      @volatile var evicted = false

      val stage = newStage(None, None, (key: String, value: String) => evicted = key == "foo2" && value == "bar2", 1)
      stage.put("foo2", "bar2")
      stage.put("bar2", "baz2")

      (evicted must eventually (beTrue))

      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    "evict when stage is flushed" in{
      @volatile var evicted = false

      val stage = newStage(Some(1), None, (key: String, value: String) => evicted = key == "foo3" && value == "bar3")
      stage.put("foo3", "bar3")
      stage.flushAll(timeout)

      (evicted must eventually (beTrue))

      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    "evict automatically" in{
      @volatile var evicted = false

      val stage = newStage(Some(10), None, (key: String, value: String) => evicted = key == "foo4" && value == "bar4")
      stage.put("foo4", "bar4")

      (evicted must eventually (beTrue))

      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    "evict automatically more then one time" in{
      @volatile var evictCount = 0

      val stage = newStage(Some(10), None, (key: String, value: String) => if (key == "foo" && value == "bar") evictCount += 1 )

      stage.put("foo", "bar")

      (evictCount must eventually (beEqualTo(1)))

      stage.put("foo", "bar")

      (evictCount must eventually (beEqualTo(2)))

      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    // this test runs for a really long time
    "evict all messages when multiple threads send messages" in {
      val messagesCount = 100
      val threadsCount  = 20

      @volatile var collected = 0
      val stage     = newStage(Some(10), None, (key: String, value: String) => collected = collected + (value.length / key.length))
      val actors    = List.fill(threadsCount) {
        defaultActorSystem.actorOf(Props(new MessageActor("1", "1", messagesCount, stage)))
      }

      val futures   = Future.sequence(actors.map(actor => (actor ? ("Send", timeout)).mapTo[Unit]))
      futures.value must eventually(200, 300.milliseconds) (beSome)

      val flushFuture = stage.flushAll(timeout)
      flushFuture.value must eventually (beSome)

      collected mustEqual(messagesCount * threadsCount)

      actors.foreach(_ ! _root_.akka.actor.PoisonPill)
      
      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }

    // this test runs for a really long time
    "evict all messages when multiple threads send messages with different keys" in {
      val messagesCount           = 50
      val threadsPerMessagesType  = 10
      val threadsCount            = 20

      val messages: List[List[String]]  = List.range(0, threadsCount) map {i => for (j <- 0 until threadsPerMessagesType) yield (List(i.toString)) } flatten

      val collected = new scala.collection.mutable.HashMap[String, Int]()
      val stage     = newStage(Some(10), None, {(key: String, value: String) =>
        val count = collected.get(key) match {
          case Some(x) => x
          case None => 0
        }
        collected.put(key, count + (value.length / key.length))
      })
      val actors    = messages map {msgs =>
        defaultActorSystem.actorOf(Props(new MessageActor(msgs(0), msgs(0), messagesCount, stage)))
      }

      val futures = Future.sequence(actors.map(actor => (actor ? ("Send", timeout)).mapTo[Unit]))
      (futures.value must eventually(500, 300.milliseconds) (beSome))

      val flushFuture = stage.flushAll(timeout)
      (flushFuture.value must eventually (beSome))

      collected mustEqual(Map[String, Int](messages.distinct.map(v => (v(0), threadsPerMessagesType * messagesCount)): _*))

      actors.foreach(_ ! _root_.akka.actor.PoisonPill)
      
      // because the eventually matcher reevaluates the LHS, use two lines so that stop only is called once
      val stopFuture = stage.stop(timeout)
      stopFuture.isCompleted must eventually (beTrue)
    }
  }

  private def newStage[T](timeToIdle: Option[Long], timeToLive: Option[Long], evict: (String, String) => Unit, capacity: Int = 10) = {
    Stage[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, evict)
  }

  class MessageActor(key: String, message: String, size: Int, stage: Stage[String, String]) extends Actor{
    def receive = {
      case "Send" => {
        for (j <- 0 until size){
          Thread.sleep(random.nextInt(100))
          stage.put(key, message)
        }

        sender ! ()
      }
      case _ =>
    }
  }
}
