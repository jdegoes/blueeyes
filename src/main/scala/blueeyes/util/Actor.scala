package blueeyes.util

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

  case class Entry[A, B](f: A => B, works: BlockingQueue[(A, Future[B])])

  implicit val actorExecutionStrategy = new ActorExecutionStrategy {
    import java.util.concurrent.locks.{ReentrantReadWriteLock => RWLock}

    /*

    John's recommendations:

    type ActorFn = Function[_, _]

    val assignments: ConcurrentMap[ActorFn, StrategyWorker]

    ...
    In SUBMIT:


    var exit = false

    while (!exit) {
      val newWorker = new StrategyWorker(f)

      if (assignments.putIfAbsent(actorFn, newWorker) == newWorker) {
        executor.execute(newWorker)
      }

      try {
        // Subtle race condition here -- it's possible we get
        // a reference to the queue IMMEDIATELY before it's
        // removed by a strategy worker that thinks it's done.

        // Unresolved question: What's the best way to do this
        // without undue synchronization??????

        assignments.get(actorFn).offer(work)

        exit = true
      }
      catch {
        case e: NullPointerException =>
      }
    }

    ...
    StrategyWorker has ReadWriteLock called "doneLock"
      case class StrategyWorker(actorFn: ActorFn) {
        val queue: BlockingQueue[(_, Future[_])]

        var done = false

        def offer(work: (_, Future[_])) = {
          doneLock.readLock {
            if (!done) queue.offer(work)
            else throw new NullPointerException()
          }
        }

        def run() = {
          while (!done) {
            // TODO: This time constant is super important!!!!!

            val head = queue.poll(1, TimeUnit.MILLISECONDS)

            if (head != null) {
              sequential.actorExecutionStrategy.submit(actorFn, head)
            }
            else {
              // We are done -- We think!!!
              doneLock.writeLock {
                // Double check to make sure something hasn't been added in
                // between last check and now:
                if (queue.size == 0) {
                  done = true
                }
              }
            }
          }
        }
      }


    When StrategyWorker is DONE:

      assignments.remove(actorFn, this)

    NOTE!!!!!!!

      Time that StrategyWorker waits for message has huge impact on performance:

      1. If time is too low, worker will miss messages, resulting in more
         workers being created than necessary

      2. If time is too high, then worker will waste time, resulting in fewer
         actor operations being completed and possibly resulting in work starvation

      Probably, it's better to err on the side of (1), using a very low timeout.
    */

    val queues            = new scala.collection.mutable.HashMap[_ => _, Entry[_, _] ]()
    val unassignedQueues  = new LinkedBlockingQueue[Entry[_, _]]()
    private val createLock              = new RWLock()

    private val sequential = new ActorExecutionStrategySequential { }

    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      addToQueue(f, work)

      execute
    }

    private def addToQueue[A, B](f: A => B, work: (A, Future[B])){
      writeLock {
        val (entry, isNew) = queues.get(f) match{
          case Some(e) => (e.asInstanceOf[Entry[A, B]], false)
          case None    => (new Entry(f, new LinkedBlockingQueue[(A, Future[B])]()), true)
        }
        entry.works.offer(work)
        if (isNew){
          unassignedQueues.offer(entry)
          queues.put(f, entry)
        }
      }
    }

    private[ActorExecutionStrategyMultiThreaded] def execute = executorService.execute(new StrategyWorker())

    class StrategyWorker extends Runnable {
      def run = {
        fetchNextQueue match {
          case Some(entry) => {
            submitWorks(entry)
            putQueueBack(entry)
          }
          case _ =>
        }
      }

      private def submitWorks[A, B](entry: Entry[A, B]){
        while (entry.works.peek != null){
          sequential.actorExecutionStrategy.submit(entry.f, entry.works.poll())
        }
      }

      private def putQueueBack[A, B](entry: Entry[A, B]){
        writeLock{
          if (entry.works.peek != null) {
            unassignedQueues.offer(entry)
            execute
          }
          else queues.remove(entry.f)
        }
      }

      private def fetchNextQueue = {
        val entry = unassignedQueues.poll( 10, TimeUnit.SECONDS )
        if (entry != null) Some(entry) else None
      }
    }

    private def writeLock[S](f: => S): S = {
      createLock.writeLock.lock()
      try {
        f
      }
      finally {
        createLock.writeLock.unlock()
      }
    }
  }
}

trait ActorExecutionStrategyFixedPool extends ActorExecutionStrategyMultiThreaded {
  def actorExecutionStrategyThreadPoolSize: Int

  lazy val executorService = Executors.newFixedThreadPool(actorExecutionStrategyThreadPoolSize)
}

object ActorExecutionStrategy extends ActorExecutionStrategyFixedPool {
  val actorExecutionStrategyThreadPoolSize = Runtime.getRuntime.availableProcessors
}

sealed trait Actor[A, B] extends PartialFunction[A, Future[B]] { self =>
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
  (implicit ActorExecutionStrategy: ActorExecutionStrategy) extends (S => Actor[A, B]) {
  def apply(state: S): Actor[A, B] = Actor.apply(state)(factory)

  def bind(state: S): () => Actor[A, B] = () => apply(state)
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