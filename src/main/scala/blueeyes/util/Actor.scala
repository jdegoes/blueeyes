package blueeyes.util

import java.util.concurrent.{Executors, ConcurrentHashMap, ConcurrentMap, BlockingQueue, LinkedBlockingQueue, TimeUnit}

trait Strategy {
  def submit[A, B](f: A => B, work: (A, Future[B])): Unit
}

trait StrategySequential {
  implicit val strategy = new Strategy {
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

trait StrategyThreaded1 {
  private val sequential = new StrategySequential { }
  private val executor = Executors.newSingleThreadExecutor

  implicit val strategy = new Strategy {
    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      executor.execute(new Runnable {
        def run = sequential.strategy.submit(f, work)
      })
    }
  }
}

trait StrategyThreadedN {
  import java.util.concurrent.ExecutorService

  def executorService: ExecutorService

  case class Entry[A, B](f: A => B, works: BlockingQueue[(A, Future[B])])

  implicit val strategy = new Strategy {
    import java.util.concurrent.locks.{ReentrantReadWriteLock => RWLock}

    val queues            = new scala.collection.mutable.HashMap[_ => _, Entry[_, _] ]()
    val unassignedQueues  = new LinkedBlockingQueue[Entry[_, _]]()
    private val createLock              = new RWLock()

    private val sequential = new StrategySequential { }

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

    private[StrategyThreadedN] def execute = executorService.execute(new StrategyWorker())

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
          sequential.strategy.submit(entry.f, entry.works.poll())
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

sealed trait Actor[A, B] extends PartialFunction[A, Future[B]] { self =>
  def map[BB](f: B => BB): Actor[A, BB] = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).map(f)
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
}

object Actor {
  /**
   *
   * {{{
   * val actor = Actor(MyActorState(x, y, z)) { state =>
   *   case MyMessage1(x, y, z) => x + y * z
   *   case MyMessage2(x)       => x
   * }
   * }}}
   */
  def apply[A, B, S](state: => S)(implicit strategy: Strategy) = (factory: S => PartialFunction[A, B]) => {
    val createdState = state

    apply[A, B](factory(createdState))
  }

  def apply[A, B](f: PartialFunction[A, B], onError: Throwable => Unit = error => ())(implicit strategy: Strategy): Actor[A, B] = new Actor[A, B] { self =>
    def isDefinedAt(request: A): Boolean = {
      try f.isDefinedAt(request)
      catch {
        case e1 =>
          try onError(e1)
          catch {
            case e2 => e2.printStackTrace
          }

          false
      }
    }

    def apply(request: A): Future[B] = {
      val response = new Future[B]

      if (!isDefinedAt(request)) {
        response.cancel(new Exception("This actor does not handle the message " + request))
      }
      else {
        strategy.submit(f, (request, response))
      }

      response
    }
  }
}