package blueeyes.concurrent.benchmark

import java.util.concurrent.CountDownLatch
import blueeyes.concurrent._
import blueeyes.concurrent.ActorStrategy._

object BenchmarkBlueeyes extends BenchmarkScenario[BenchmarkActor]{

  def sendMessage(actor: BenchmarkActor, message: String) = actor.processor(message)

  def shutdown = {}

  def createActor(index: Int, actors: Array[BenchmarkActor], cdl: CountDownLatch) = actor(index, actors, cdl)

  def array(count: Int) = new Array[BenchmarkActor](count)

  private def actor(index: Int, actors: Array[BenchmarkActor], cdl: CountDownLatch) = new BenchmarkActor(index, actors, cdl)
}

class BenchmarkActor(index: Int, actors: Array[BenchmarkActor], cdl: CountDownLatch) extends Actor{
  val processor: (String) => Future[Unit] = lift1({x: String =>
    if (index < actors.length - 1)
      actors(index + 1).processor(x)
    cdl.countDown()
  })
}
