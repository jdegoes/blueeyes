package blueeyes.concurrent

import java.util.concurrent.{Executors, ConcurrentHashMap, ConcurrentMap, BlockingQueue, LinkedBlockingQueue, TimeUnit}

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

private[concurrent] case class StrategyWorker[A, B](actorFn: A => B, assignments: ConcurrentMap[_ => _, StrategyWorker[_, _]]) extends Runnable{
  private val sequential = new ActorExecutionStrategySequential { }

  val doneLock = new ReadWriteLock{}
  val queue: BlockingQueue[(A, Future[B])] = new LinkedBlockingQueue[(A, Future[B])]()

  var done = false

  def offer(work: (A, Future[B])) = {
    doneLock.readLock {
      if (!done) queue.offer(work)
      else throw new NullPointerException()
    }
  }

  def run() = {
    while (!done) {
      val head = queue.poll(1, TimeUnit.MILLISECONDS)

      if (head != null) {
        sequential.actorExecutionStrategy.submit(actorFn, head)
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


trait ActorExecutionStrategyFixedPool extends ActorExecutionStrategyMultiThreaded {
  def actorExecutionStrategyThreadPoolSize: Int

  lazy val executorService = Executors.newFixedThreadPool(actorExecutionStrategyThreadPoolSize)
}

object ActorExecutionStrategy extends ActorExecutionStrategyMultiThreaded {
  lazy val executorService = Executors.newCachedThreadPool()
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

object Actor {
  /**
   *
   * {{{
   * val actor = Actor(database) { database =>
   *   case MyMessage1(x, y, z) => database.lookup(x, y, z)
   *   case MyMessage2(x)       => database.save(x)
   * }
   * }}}
   */
  def apply[A, B, S](state: => S)(factory: S => PartialFunction[A, B])(implicit ActorExecutionStrategy: ActorExecutionStrategy): Actor[A, B] = {
    val createdState = state

    apply[A, B](factory(createdState))
  }

  def constant[A, B](b: B): Actor[A, B] = new Actor[A, B] {
    def isDefinedAt(a: A): Boolean = true

    def apply(a: A): Future[B] = Future.lift(b)
  }

  def apply[A, B](f: PartialFunction[A, B])(implicit ActorExecutionStrategy: ActorExecutionStrategy): Actor[A, B] = new Actor[A, B] { self =>
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
        ActorExecutionStrategy.submit(f, (request, response))
      }

      response
    }
  }
}