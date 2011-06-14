package blueeyes
package persistence.cache

import org.specs.Specification
import org.specs.util.TimeConversions._
import java.util.concurrent.TimeUnit.{MILLISECONDS}

import blueeyes.concurrent.Future
import blueeyes.concurrent.FutureImplicits._

import scala.util.Random
import scalaz._
import Scalaz._
import akka.actor.Actor

class StageSpec extends Specification{
  private val random    = new Random()
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + s2
  }

  "Stage" should {
    "evict when entry is expired" in {
      @volatile var evicted = false

      val stage = newStage(None, Some(20),
        (key: String, value: String) => {
          evicted = evicted || (key == "foo" && value == "bar")
        }
      )

      stage.put("foo", "bar")
      stage.put("bar", "baz")

      evicted must eventually (be (true))

      val stop = stage.stop
      stop.isDone must eventually (be (true))
    }

    "evict when stage is over capacity" in{
      @volatile var evicted = false

      val stage = newStage(None, None, {(key: String, value: String) => evicted = key == "foo2" && value == "bar2"}, 1)
      stage.put("foo2", "bar2")
      stage.put("bar2", "baz2")

      evicted must eventually (be (true))
      
      val stop = stage.stop
      stop.isDone must eventually (be (true))
    }

    "evict when stage is flushed" in{
      @volatile var evicted = false

      val stage = newStage(Some(1), None, {(key: String, value: String) => evicted = key == "foo3" && value == "bar3"})
      stage.put("foo3", "bar3")
      stage.flushAll

      evicted must eventually (be (true))

      val stop = stage.stop
      stop.isDone must eventually (be (true))
    }

    "evict automatically" in{
      @volatile var evicted = false

      val stage = newStage(Some(10), None, {(key: String, value: String) => evicted = key == "foo4" && value == "bar4"})
      stage.put("foo4", "bar4")

      evicted must eventually (be (true))

      stage.flushAll

      val stop = stage.stop
      stop.isDone must eventually (be (true))
    }

    "evict automatically more then one time" in{
      @volatile var evictCount = 0

      val stage = newStage(Some(10), None, {(key: String, value: String) => if (key == "foo" && value == "bar") evictCount += 1 })
      
      stage.put("foo", "bar")

      evictCount must eventually (beEqualTo(1))

      stage.put("foo", "bar")

      evictCount must eventually (beEqualTo(2))

      val stop = stage.stop
      stop.isDone must eventually (be (true))
    }

    "evict all messages when multiple threads send messages" in{
      val messagesCount = 100
      val threadsCount  = 20

      @volatile var collected = 0
      val stage     = newStage(Some(10), None, {(key: String, value: String) => collected = collected + (value.length / key.length)})
      val actors    = List.range(0, threadsCount) map {i =>
        val actor = Actor.actorOf(new MessageActor("1", "1", messagesCount, stage))
        actor.start()
        actor
      }

      val futures   = Future((actors map {actor => akkaFutureToFuture(actor.!!![Unit]("Send"))}): _*)
      futures.value must eventually(200, 300.milliseconds) (beSomething)

      val flushFuture = stage.flushAll
      flushFuture.value must eventually (beSomething)

      collected mustEqual(messagesCount * threadsCount)

      val stop = stage.stop
      stop.isDone must eventually (be (true))

      actors.foreach(_.stop())
    }

    "evict all messages when multiple threads send messages with different keys" in{
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
        val actor = Actor.actorOf(new MessageActor(msgs(0), msgs(0), messagesCount, stage))
        actor.start()
        actor
      }

      val futures = Future((actors map {actor => akkaFutureToFuture(actor.!!![Unit]("Send"))}): _*)
      futures.value must eventually(200, 300.milliseconds) (beSomething)

      val flushFuture = stage.flushAll
      flushFuture.value must eventually (beSomething)

      collected mustEqual(Map[String, Int](messages.distinct.map(v => (v(0), threadsPerMessagesType * messagesCount)): _*))

      val stop = stage.stop
      stop.isDone must eventually (be (true))

      actors.foreach(_.stop())
    }
  }

  private def newStage[T](
    timeToIdle: Option[Long] = None,
    timeToLive: Option[Long] = None,
    evict:      (String, String) => Unit,
    capacity:   Int = 10) = {

    Stage[String, String](
      ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, 
      (s1: String, s2: String) => { evict(s1, s2) }
    )
  }

  class MessageActor(key: String, message: String, size: Int, stage: Stage[String, String]) extends Actor{
    def receive = {
      case "Send" =>
        for (j <- 0 until size){
          Thread.sleep(random.nextInt(100))
          stage.put(key, message)
        }
        self.reply(())
      case _ =>
    }
  }
}
