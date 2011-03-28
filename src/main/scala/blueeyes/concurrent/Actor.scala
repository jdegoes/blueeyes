package blueeyes.concurrent

import scalaz.{Success, Validation}
import java.util.concurrent.{Executors, ConcurrentHashMap, ConcurrentMap, ThreadPoolExecutor, BlockingQueue, SynchronousQueue, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicLong

trait ActorExecutionStrategy {
  def submit[A, B](f: A => B, work: (A, Future[B])): Unit
}

trait ActorExecutionStrategySequential {
  implicit val actorExecutionStrategy = new ActorExecutionStrategy {
    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      val (request, response) = work

      try {
        response.deliver(f(request))
      }
      catch {
        case e => response.cancel(e)
      }
    }
  }
}

trait ActorExecutionStrategySingleThreaded {
  private val sequential = new ActorExecutionStrategySequential { }
  private val executor = Executors.newSingleThreadExecutor

  implicit val actorExecutionStrategy = new ActorExecutionStrategy {
    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      executor.execute(new Runnable {
        def run = sequential.actorExecutionStrategy.submit(f, work)
      })
    }
  }
}

trait ActorExecutionStrategyMultiThreaded {
  import java.util.concurrent.ExecutorService

  def executorService: ExecutorService

  type ActorFn = Function[_, _]

  implicit val actorExecutionStrategy = new ActorExecutionStrategy {

  val assignments = new ConcurrentHashMap[ActorFn, StrategyWorker[_, _]]

  def submit[A, B](actorFn: A => B, work: (A, Future[B])): Unit = {
    var exit = false

    while (!exit) {
      val newWorker = new StrategyWorker(actorFn, assignments)

      if (assignments.putIfAbsent(actorFn, newWorker) == null) {
        executorService.execute(newWorker)
      }

      try {
        assignments.get(actorFn).asInstanceOf[StrategyWorker[A, B]].offer(work)

        exit = true
      }
      catch {
        case e: NullPointerException =>
      }
    }
  }
  }
}

private[concurrent] class StrategyWorker[A, B](actorFn: A => B, assignments: ConcurrentMap[_ => _, StrategyWorker[_, _]]) extends Runnable{
  import StrategyWorker._
  private val executionSequential = new ActorExecutionStrategySequential { }
  private val deliverySequential  = new FutureDeliveryStrategySequential { }
  @volatile
  private var deliveryCount       = 0

  val doneLock = new ReadWriteLock{}
  val queue: BlockingQueue[StrategyWorkerTask] = new LinkedBlockingQueue[StrategyWorkerTask]()

  var done = false

  def offer(work: (A, Future[B])): Unit = offer(Work(work))

  def offer[T](value: T, listeners: Iterable[T => Unit], errorHandler: List[Throwable] => Unit): Unit = offer(Deliver(value, listeners, errorHandler))


  def waitDelivery = deliveryCount = deliveryCount + 1

  private def offer(task: StrategyWorkerTask): Unit = {
    doneLock.readLock {
      if (!done) queue.offer(task)
      else throw new NullPointerException()
    }
  }

  def run() = {
    ActorContext.withWorker(this){
      while (!done) {
        val head = queue.poll(1, TimeUnit.MILLISECONDS)

        if (head != null) {
          head match{
            case w: Work[A, B] => executionSequential.actorExecutionStrategy.submit(actorFn, w.work)
            case d: Deliver[_] =>
              deliverySequential.futureDeliveryStrategy.deliver(d.value, d.listeners, d.errorHandler)
              deliveryCount = deliveryCount - 1
          }
        }
        else {
          doneLock.writeLock {
            if (queue.size == 0 && deliveryCount == 0) {
              done = true
              assignments.remove(actorFn, this)
            }
          }
        }
      }
    }
  }

  private[StrategyWorker] sealed trait StrategyWorkerTask
  private[StrategyWorker] case class Work[A, B](work: (A, Future[B])) extends StrategyWorkerTask
  private[StrategyWorker] case class Deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) extends StrategyWorkerTask
}

private[concurrent] object StrategyWorker{
  object ActorContext{
    private val tl = new ThreadLocal[StrategyWorker[_, _]]

    def get : Option[StrategyWorker[_, _]] = {
      if (tl.get() != null) Some(tl.get()) else None
    }

    def withWorker(x : StrategyWorker[_, _])(f : => Unit) = {
      val old = get
      try {
        tl.set(x)
        f
      } finally {
        tl.set(old.getOrElse(null))
      }
    }
  }
}

trait ActorExecutionStrategyFixedPool extends ActorExecutionStrategyMultiThreaded {
  def actorExecutionStrategyThreadPoolSize: Int

  lazy val executorService = Executors.newFixedThreadPool(actorExecutionStrategyThreadPoolSize)
}

object ActorExecutionStrategy extends ActorExecutionStrategyMultiThreaded {
  lazy val executorService = new ThreadPoolExecutor(2, 1000, 10*60, TimeUnit.SECONDS, new SynchronousQueue())
}

sealed trait Actor[A, B] extends PartialFunction[A, Future[B]] { self =>
  def ! (message: A): Future[B] = self.apply(message)

  def orElse[A1 <: A, B1 >: B](that: Actor[A1, B1]): Actor[A1, B1] = new Actor[A1, B1] {
    def isDefinedAt(a: A1): Boolean = self.isDefinedAt(a) || that.isDefinedAt(a)

    // TODO: Simplify when added covariance for Futures:
    def apply(a: A1): Future[B1] = if (self.isDefinedAt(a)) self.apply(a).map(v => v: B1) else that.apply(a).map(v => v: B1)
  }

  def map[BB](f: B => BB): Actor[A, BB] = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).map(f)
  }

  def flatMap[BB](f: B => Future[BB]) = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).flatMap(f)
  }

  /** Actor composition.
   *
   * {{{
   * val aToC = aToB >>> bToC
   * }}}
   */
  def >>> [BB](that: Actor[B, BB]): Actor[A, BB] = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).flatMap(that)
  }

  /** Flattens an actor from A => Future[B] to an actor from A => B.
   */
  def flatten[BB](implicit witness: B => Future[BB]): Actor[A, BB] = flatMap(witness)
}

sealed case class ActorFactory[A, B, S](factory: S => PartialFunction[A, B])
  (implicit actorExecutionStrategy: ActorExecutionStrategy) { self =>
  def apply(state: => S): Actor[A, B] = Actor.apply(state)(factory)

  def bind(state: => S): () => Actor[A, B] = () => self.apply(state)
}

trait ActorImplementation {
  implicit def actorExecutionStrategy: ActorExecutionStrategy

  implicit def futureDeliveryStrategy: FutureDeliveryStrategy

  def apply[A, B, S](state: => S)(factory: S => PartialFunction[A, B]): Actor[A, B] = {
    val createdState = state

    apply[A, B](factory(createdState))
  }

  def constant[A, B](b: B): Actor[A, B] = new Actor[A, B] {
    def isDefinedAt(a: A): Boolean = true

    def apply(a: A): Future[B] = Future.lift(b)
  }

  def apply[A, B](f: PartialFunction[A, B]): Actor[A, B] = new Actor[A, B] { self =>
    def isDefinedAt(request: A): Boolean = {
      try f.isDefinedAt(request)
      catch {
        case e => e.printStackTrace; false
      }
    }

    def apply(request: A): Future[B] = {
      val response = new Future[B]

      if (!isDefinedAt(request)) {
        response.cancel(new Exception("This actor does not handle the message " + request))
      }
      else {
        actorExecutionStrategy.submit(f, (request, response))
      }

      response
    }
  }
}

trait ActorImplementationSequential extends ActorImplementation{
  private lazy val futureDeliverySequential = new FutureDeliveryStrategySequential{}
  private lazy val actorExecutionSequential = new ActorExecutionStrategySequential{}

  implicit def futureDeliveryStrategy = futureDeliverySequential.futureDeliveryStrategy

  implicit def actorExecutionStrategy = actorExecutionSequential.actorExecutionStrategy
}

trait ActorImplementationMultiThreaded extends ActorImplementation{
  private val sequential = new FutureDeliveryStrategySequential{}

  implicit def futureDeliveryStrategy = StrategyWorker.ActorContext.get.map(worker => new FutureDeliveryStrategyWorker(worker)).getOrElse(sequential.futureDeliveryStrategy)

  implicit def actorExecutionStrategy = ActorExecutionStrategy.actorExecutionStrategy

  private[ActorImplementationMultiThreaded] class FutureDeliveryStrategyWorker(worker: StrategyWorker[_, _]) extends FutureDeliveryStrategy{
   worker.waitDelivery
   def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) {
     worker.offer(value, listeners, errorHandler)
   }
  }
}

object Actor extends ActorImplementationMultiThreaded{
  implicit def actorOfFutureToFlattenedActor[A, B](a: Actor[A, Future[B]]): Actor[A, B] = a.flatten
}