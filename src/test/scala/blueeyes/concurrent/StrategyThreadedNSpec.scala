package blueeyes.concurrent

import org.specs.Specification
import java.util.concurrent.Executors
import ActorStrategy._
import test.FutureMatchers
import util.Random

class StrategyThreadedNSpec extends Specification with FutureMatchers{

  private val random = new Random()
  private val actor  = new ActorImpl()

  "StrategyThreadedN: handle one request" in{
    val future = new Future[Int]()

    actorExecutionStrategy.execute1(actor)(actor.f _)(1)(future)

    future.value must eventually (beSome(3))
  }

  "StrategyThreadedN: handle multiple requests" in{
    val futures = List.fill(100) {new Future[Int]()}

    val fun = actor.f _

    futures foreach { future =>
      actorExecutionStrategy.execute1(actor)(fun)(1)(future)
    }

    futures foreach { future =>
      future.value must eventually(beSome(3))
    }
  }

  "StrategyThreadedN: handle multiple requests from multiple threads for multiple functions" in{
    val executor = Executors.newFixedThreadPool(40)

    val functions  = Array.fill(2){actor.f _}
    val entries    = List.range(0, 300) map { i => functions(i % functions.size) } map { (_, new Future[Int]()) }
    val futures    = entries.map(_._2)

    entries foreach { f =>
      executor execute(new Runnable{
        def run = {
          Thread.sleep(random.nextInt(150))

          actorExecutionStrategy.execute1(actor)(f._1)(1)(f._2)

          actorExecutionStrategy.assignments.size must beLessThan (functions.size + 1)
        }
      })
    }

    futures foreach { future =>
      future.value must eventually(beSome(3))
    }

    actorExecutionStrategy.assignments.size must be (0)
  }

  class ActorImpl extends Actor{
    def f(a: Int):  Int = {
      Thread.sleep(random.nextInt(20))
      a + 2
    }
  }
}