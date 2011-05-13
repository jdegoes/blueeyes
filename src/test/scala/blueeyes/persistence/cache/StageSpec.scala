package blueeyes.persistence.cache

import org.specs.Specification
import org.specs.util.{Duration => SpecsDuration}
import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch

import util.Random
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
    "evict when entry is expired" in{
      var evicted = false

      val stage = newStage(None, Some(200), {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")
      Thread.sleep(400)
      stage.put("bar", "baz")

      evicted must eventually (be (true))
    }
    "evict when stage is over capacity" in{
      var evicted = false

      val stage = newStage(None, None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"}, 1)
      stage.put("foo", "bar")
      stage.put("bar", "baz")

      evicted must eventually (be (true))
    }
    "evict when stage is flushed" in{
      var evicted = false

      val stage = newStage(Some(1), None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")
      stage.flushAll

      evicted must eventually (be (true))
    }
    "evict automatically" in{
      var evicted = false

      val stage = newStage(Some(1000), None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")

      Thread.sleep(1000)

      evicted must eventually (be (true))
    }
    "evict automatically more then one time" in{
      var evicted = false

      val stage = newStage(Some(1000), None, {(key: String, value: String) => evicted = key == "foo" && value == "bar"})
      stage.put("foo", "bar")
      Thread.sleep(1000)
      evicted = false

      stage.put("foo", "bar")
      Thread.sleep(1000)

      evicted must eventually (be (true))
    }

    "evict all messages when multiple threads send messages" in{
      val messagesCount = 100
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
      val messagesCount = 100
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
    evict:      (String, String) => Unit = {(key: String, value: String) => () },
    capacity:   Int = 100) = {

    Stage[String, String](ExpirationPolicy(timeToIdle, timeToLive, MILLISECONDS), capacity, evict)
  }

  class MessageActor(key: String, messages: Array[String], stage: Stage[String, String]) extends Actor{
    def send = lift{ () =>
      messages foreach { message =>
        Thread.sleep(random.nextInt(100))
        stage.put(key, message)
      }
    }
  }
}
