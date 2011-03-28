package blueeyes.concurrent.benchmark

import java.util.concurrent.{TimeUnit, CountDownLatch}
import blueeyes.concurrent._
import actors.Scheduler

object BenchmarkBlueeyes extends BenchmarkScenario[Actor[String, Unit]]{

  def sendMessage(actor: Actor[String, Unit], message: String) = actor ! message

  def shutdown = {}

  def createActor(index: Int, actors: Array[Actor[String, Unit]], cdl: CountDownLatch) = actor(index, actors, cdl)

  def array(count: Int) = new Array[Actor[String, Unit]](count)

  private def actor(index: Int, actors: Array[Actor[String, Unit]], cdl: CountDownLatch) = Actor.actor[String, Unit, Tuple3[Int, Array[Actor[String, Unit]], CountDownLatch]]((index, actors, cdl)){ state => {
    case x: String =>
      val (index, actors, cdl) = state
      if (index < actors.length - 1)
        actors(index + 1) ! x
      cdl.countDown()
  }}
}