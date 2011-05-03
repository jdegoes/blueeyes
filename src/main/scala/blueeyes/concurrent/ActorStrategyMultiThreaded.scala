package blueeyes.concurrent

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, ThreadPoolExecutor, BlockingQueue, SynchronousQueue, LinkedBlockingQueue, TimeUnit}

object ActorStrategyMultiThreaded{
  private lazy val executorService = new ThreadPoolExecutor(2, 1000, 10*60, TimeUnit.SECONDS, new SynchronousQueue())
}

trait ActorStrategyMultiThreaded extends ActorStrategy{

  private val executionSequential = new ActorExecutionStrategySequential2 { }

  private val futureSequential = new FutureDeliveryStrategySequential{}

  implicit def futureDeliveryStrategy = ActorContext.get.map(actorFn => new FutureDeliveryStrategyWorker(actorFn)).getOrElse(futureSequential.futureDeliveryStrategy)

  implicit lazy val actorExecutionStrategy = new MultiThreadedExecutionStrategy()

  private[ActorStrategyMultiThreaded] class MultiThreadedExecutionStrategy extends ActorExecutionStrategy2{

    val assignments = new ConcurrentHashMap[AnyRef, StrategyWorker]

    def execute1[T1, R](f: (T1) => R)(v1: T1)(response: Future[R]) = submit0(f, Work1(f, v1, response))

    def execute2[T1, T2, R](f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]) = submit0(f, Work2(f, v1, v2, response))

    def submit0(actorFn: AnyRef, strategyTask: StrategyWorkerTask): Unit = {
      var exit = false

      while (!exit) {
        val newWorker = new StrategyWorker(actorFn, assignments)

        if (assignments.putIfAbsent(actorFn, newWorker) == null) {
          ActorStrategyMultiThreaded.executorService.execute(newWorker)
        }

        try {
          assignments.get(actorFn).asInstanceOf[StrategyWorker].offer(strategyTask)

          exit = true
        }
        catch {
          case e: NullPointerException =>
        }
      }
    }
  }

  private[ActorStrategyMultiThreaded] class StrategyWorker(actorFn: AnyRef, assignments: ConcurrentMap[AnyRef, StrategyWorker]) extends Runnable{

    val doneLock = new ReadWriteLock{}
    val queue: BlockingQueue[StrategyWorkerTask] = new LinkedBlockingQueue[StrategyWorkerTask]()

    var done = false

    def offer(strategyTask: StrategyWorkerTask): Unit = {
      doneLock.readLock {
        if (!done) queue.offer(strategyTask)
        else throw new NullPointerException()
      }
    }

    def run() = {
      ActorContext.withActorFn(actorFn){
        while (!done) {
          val head = queue.poll(1, TimeUnit.MILLISECONDS)

          if (head != null) {
            head.apply()
          }
          else {
            doneLock.writeLock {
              if (queue.size == 0) {
                done = true
                assignments.remove(actorFn, this)
              }
            }
          }
        }
      }
    }
  }

  private[ActorStrategyMultiThreaded] class FutureDeliveryStrategyWorker[R](actorFn: AnyRef) extends FutureDeliveryStrategy{
   def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) {
     actorExecutionStrategy.submit0(actorFn, Deliver(value, listeners, errorHandler))
   }
  }

  object ActorContext{
    private val tl = new ThreadLocal[AnyRef]

    def get : Option[AnyRef] = {
      if (tl.get() != null) Some(tl.get()) else None
    }

    def withActorFn(actorFn: AnyRef)(f : => Unit) = {
      val old = get
      try {
        tl.set(actorFn)
        f
      } finally {
        tl.set(old.getOrElse(null))
      }
    }
  }

  private[ActorStrategyMultiThreaded] sealed trait StrategyWorkerTask extends Function0[Unit]
  private[ActorStrategyMultiThreaded] case class Work1[T1, R](f: T1 => R, v1: T1, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute1(f)(v1)(response)
    }
  }

  private[ActorStrategyMultiThreaded] case class Work2[T1, T2, R](f: (T1, T2) => R, v1: T1, v2: T2, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute2(f)(v1, v2)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Deliver[R](value: R, listeners: Iterable[R => Unit], errorHandler: List[Throwable] => Unit) extends StrategyWorkerTask{
    def apply() {
      futureSequential.futureDeliveryStrategy.deliver(value, listeners, errorHandler)
    }
  }
}

