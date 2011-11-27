package blueeyes.persistence.cache

import java.util.concurrent.TimeUnit.{MILLISECONDS}
import java.util.concurrent.CountDownLatch
import scala.util.Random
import scalaz.Semigroup
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import org.specs2.time.TimeConversions._
import akka.actor.Actor

object StageProfile{
  private val random    = new Random()
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: String, s2: => String) = s1 + s2
  }

  def main(args: Array[String]){
    implicit val timeout = Actor.Timeout(100000)
    println("START")
    Thread.sleep(10000)
    println("REAL START")
    val messagesCount           = 500
    val threadsPerMessagesType  = 5
    val threadsCount            = 5

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

    val futures = Future((actors map {actor => actor.?("Send").mapTo[Unit].toBlueEyes}): _*)
    awaitFuture(futures)

//    val flushFuture = stage.flushAll
//    awaitFuture(flushFuture)

    println("STOPPED")
    Thread.sleep(30000)
    println("REAL STOPPED")

    actors.foreach(_.stop())
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
