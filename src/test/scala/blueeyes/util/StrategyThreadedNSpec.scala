package blueeyes.util

import org.spex.Specification
import java.util.concurrent.{CountDownLatch, Executors}
import util.Random

class StrategyThreadedNSpec extends Specification{

  private val random = new Random()

  private val strategy = new StrategyThreadedN{
    val executorService = Executors.newFixedThreadPool(2)
  }

  "StrategyThreadedN: handle one request" in{
    val future = new Future[Int]()

    strategy.strategy.submit(f _, (1, future))

    awaitFuture(future)

    future.value mustEqual(Some(3))
  }

  "StrategyThreadedN: handle musltiple requests" in{
    val futures = List.fill(100) {new Future[Int]()}

    val fun = f _

    futures foreach { future =>
      strategy.strategy.submit(fun, (1, future))
    }

    awaitFuture(Future(futures: _*))

    futures foreach { future =>
      future.value mustEqual(Some(3))
    }
  }

  "StrategyThreadedN: handle musltiple requests from multiple threads for multiple functions" in{

    val fun  = f _
    val fun1 = f _
    val fun2 = f _
    val fun3 = f _
    val fun4 = f _

    val entries = List.range(0, 200).map { i =>
      i % 5 match {
        case 0 => fun
        case 1 => fun1
        case 2 => fun2
        case 3 => fun3
        case 4 => fun4
      }
    } map { (_, new Future[Int]()) }

    val futures = entries.map(_._2)

    val executor = Executors.newFixedThreadPool(40)

    entries foreach { f =>
      executor execute(new Runnable{
        def run = {
          Thread.sleep(random.nextInt(100))

          strategy.strategy.submit(f._1, (1, f._2))

          strategy.strategy.queues.size must beLessThan (6)
          strategy.strategy.unassignedQueues.size must beLessThan (6)
        }
      })
    }

    awaitFuture(Future(futures: _*))

    futures foreach { future =>
      future.value mustEqual(Some(3))
    }

    strategy.strategy.queues.size must be (0)
    strategy.strategy.unassignedQueues.size must be (0)
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