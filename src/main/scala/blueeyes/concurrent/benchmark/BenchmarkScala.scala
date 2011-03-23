package blueeyes.concurrent.benchmark

import java.util.concurrent.{TimeUnit, CountDownLatch}
import actors.{Scheduler, Actor}
import scala.actors._
import scala.actors.Actor._

object BenchmarkScala extends BenchmarkScenario[Actor]{

  def sendMessage(actor: Actor, message: String) = actor ! message

  def shutdown = {
    Scheduler.shutdown
  }

  def createActor(index: Int, actors: Array[Actor], cdl: CountDownLatch) = {
    val actor   = new BenchmarkActor(index, actors, cdl)
    actor.start

    actor
  }

  def array(count: Int) = new Array[Actor](count)

  class BenchmarkActor(index: Int, actors: Array[Actor], cdl: CountDownLatch) extends Actor{
    def act{
      loop {
        react {
          case x: Any =>
            if (index < actors.length - 1)
              actors(index + 1) ! x
            cdl.countDown()
        }
      }
    }
  }
}