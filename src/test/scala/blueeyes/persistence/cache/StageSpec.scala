package blueeyes
package persistence.cache

import org.specs.Specification
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

import scala.util.Random
import scalaz.Semigroup
import blueeyes.concurrent.{ActorStrategy, Actor, Future}
import scala.collection.mutable.ArrayBuilder.ofRef
import ActorStrategy._

class StageSpec extends Specification{
  private val random    = new Random()
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + s2
  }

  "Stage" should {
    "evict when entry is expired" in {
      @volatile var evicted = false

      println("Starting")

      val stage = newStage(None, Some(20), 
        (key: String, value: String) => {
          println("Evicting " + key + " : " + value)
          evicted = (key == "foo" && value == "bar")
        }
      )

      stage.put("foo", "bar")
      stage.put("bar", "baz")

      evicted must eventually (be (true))// ->- { _ => println("Finishing") }

      stage.flushAll
    }

    "evict when stage is over capacity" in{
      @volatile var evicted = false

      val stage = newStage(None, None, {(key: String, value: String) => evicted = key == "foo2" && value == "bar2"}, 1)
      stage.put("foo2", "bar2")
      stage.put("bar2", "baz2")

      evicted must eventually (be (true))
      
      stage.flushAll
    }

    "evict when stage is flushed" in{
      @volatile var evicted = false

      val stage = newStage(Some(1), None, {(key: String, value: String) => evicted = key == "foo3" && value == "bar3"})
      stage.put("foo3", "bar3")
      stage.flushAll

      evicted must eventually (be (true))
    }

    "evict automatically" in{
      @volatile var evicted = false

      val stage = newStage(Some(10), None, {(key: String, value: String) => evicted = key == "foo4" && value == "bar4"})
      stage.put("foo4", "bar4")

      evicted must eventually (be (true))

      stage.flushAll
    }

    "evict automatically more then one time" in{
      @volatile var evictCount = 0

      val stage = newStage(Some(10), None, {(key: String, value: String) => if (key == "foo" && value == "bar") evictCount += 1 })
      
      stage.put("foo", "bar")

      evictCount must eventually (beEqualTo(1))

      stage.put("foo", "bar")

      evictCount must eventually (beEqualTo(2))

      stage.flushAll
    }

    "evict all messages when multiple threads send messages" in{
      val messagesCount = 10
      val threadsCount  = 20

      val messages  = Array.fill(messagesCount)("1")
      val collected = new ofRef[String]()
      val stage     = newStage(Some(500), None, {(key: String, value: String) => collected += value})
      val actors    = List.range(0, threadsCount) map {i => new MessageActor("key", messages, stage) }
      val futures   = actors map {actor => actor.send()}

      awaitFuture(Future(futures: _*))
      val flushFuture = stage.flushAll

      flushFuture.value must eventually (beSomething)
      collected.result.mkString("") mustEqual(Array.fill(threadsCount)(messages).flatten).mkString("")
    }
    "evict all messages when multiple threads send messages with different keys" in{
      val messagesCount = 10
      val threadsCount  = 20

      val messages  = List.range(0, threadsCount) map {i => Array.fill(messagesCount)(i.toString) }
      val collected = new scala.collection.mutable.HashMap[String, ofRef[String]]()
      val stage     = newStage(Some(500), None, {(key: String, value: String) =>
        val ofRef = collected.get(key) match {
          case Some(x) => x
          case None => new ofRef[String]()
        }
        ofRef += value
        collected.put(key, ofRef)
      })
      val actors    = messages map {msgs => (new MessageActor(msgs(0), msgs, stage))}

      val futures = actors map {actor => actor.send()}
      awaitFuture(Future(futures: _*))

      val flushFuture = stage.flushAll
      flushFuture.value must eventually (beSomething)

      collected.size mustEqual(threadsCount)
      collected.mapValues(value => value.result.mkString("")) mustEqual(Map[String, String](messages.map(v => (v(0), v.mkString(""))): _*))
    }
  }

  private def awaitFuture(future: Future[_]) = {
    val countDownLatch = new CountDownLatch(1)
    future deliverTo { v =>
      countDownLatch.countDown
    }
    countDownLatch.await
  }

  private def newStage[T](
    timeToIdle: Option[Long] = None,
    timeToLive: Option[Long] = None,
    evict:      (String, String) => Unit,
    capacity:   Int = 10) = {

    Stage[String, String](
      ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, 
      (s1: String, s2: String) => { evict(s1, s2) ; println("Evicted!") }
    )
  }

  class MessageActor(key: String, messages: Array[String], stage: Stage[String, String]) extends Actor{
    def send = lift{ () =>
      messages foreach { message =>
        Thread.sleep(random.nextInt(10))
        stage.put(key, message)
      }
    }
  }
}
