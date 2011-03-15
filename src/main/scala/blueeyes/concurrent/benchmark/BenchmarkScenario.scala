package blueeyes.concurrent.benchmark

import java.util.concurrent.{TimeUnit, CountDownLatch}
import actors.Scheduler

trait BenchmarkScenario[T] {

  def main(args: Array[String]) {

    val actorsCount   = args(0).toInt
    val messagesCount = actorsCount / 2

    val cdl     = new CountDownLatch(actorsCount * messagesCount)
    val start   = System.currentTimeMillis()
    val actors  = createActors(actorsCount, cdl)

    sendMessages(actors, messagesCount, cdl)

    cdl.await(1000, TimeUnit.SECONDS)

    println("TIME=" + (System.currentTimeMillis() - start))

    Scheduler.shutdown
  }

  private def sendMessages(actors: Array[T], messagesCount: Int, cdl: CountDownLatch){
    var i = 0
    while (i < messagesCount) {
      sendMessage( actors(i) , "Hi")
      var j: Int = 0
      while (j < i) {
        cdl.countDown()
        j = j + 1
      }
      i += 1
    }
  }

  private def createActors(actorsCount: Int, cdl: CountDownLatch): Array[T] = {
    val actors  = array(actorsCount)

    var i: Int = 0
    while (i < actorsCount) {
      actors(i) = createActor(i, actors, cdl)
      i += 1
    }
    actors
  }

  def createActor(index: Int, actors: Array[T], cdl: CountDownLatch): T

  def shutdown

  def sendMessage(actor: T, message: String)

  def array(count: Int): Array[T]
}