package blueeyes.concurrent

import org.specs.Specification
import java.util.concurrent.{CountDownLatch, Executors}
import util.Random

class StrategyThreadedNSpec extends Specification with FutureDeliveryStrategySequential{

  private val random = new Random()

  private val strategy = Actor.actorExecutionStrategy

  "StrategyThreadedN: handle one request" in{
    val future = new Future[Int]()

    strategy.submit(f _, (1, future))

    awaitFuture(future)

    future.value mustEqual(Some(3))
  }

  "StrategyThreadedN: handle musltiple requests" in{
    val futures = List.fill(100) {new Future[Int]()}

    val fun = f _

    futures foreach { future =>
      strategy.submit(fun, (1, future))
    }

    awaitFuture(Future(futures: _*))

    futures foreach { future =>
      future.value mustEqual(Some(3))
    }
  }

  "StrategyThreadedN: handle multiple requests from multiple threads for multiple functions" in{
    val executor = Executors.newFixedThreadPool(40)

    val functions  = Array.fill(2){f _}
    val entries    = List.range(0, 300) map { i => functions(i % functions.size) } map { (_, new Future[Int]()) }
    val futures    = entries.map(_._2)

    entries foreach { f =>
      executor execute(new Runnable{
        def run = {
          Thread.sleep(random.nextInt(150))

          strategy.submit(f._1, (1, f._2))

          strategy.assignments.size must beLessThan (functions.size + 1)
        }
      })
    }

    awaitFuture(Future(futures: _*))

    futures foreach { future =>
      future.value mustEqual(Some(3))
    }

    strategy.assignments.size must be (0)
  }

  private def awaitFuture(future: Future[_]) = {
    val countDownLatch = new CountDownLatch(1)
    future deliverTo { v =>
      countDownLatch.countDown
    }
    countDownLatch.await
  }

  private def f(a: Int):  Int = {
    Thread.sleep(random.nextInt(20))
    a + 2
  }
}