package blueeyes.concurrent

import java.util.concurrent._

trait ActorStrategyMultiThreaded{

  def executorService: ExecutorService

  private val executionSequential = new ActorExecutionStrategySequential { }

  private val futureSequential = new FutureDeliveryStrategySequential{}

  implicit def futureDeliveryStrategy = ActorContext.get.map(actor => new FutureDeliveryStrategyWorker(actor)).getOrElse(futureSequential.futureDeliveryStrategy)

  implicit lazy val actorExecutionStrategy = new MultiThreadedExecutionStrategy()

  private[ActorStrategyMultiThreaded] class MultiThreadedExecutionStrategy extends ActorExecutionStrategy{

    val assignments = new ConcurrentHashMap[AnyRef, StrategyWorker]

    def execute[R](actor: Actor)(f: () => R)(response: Future[R]) = submit0(actor, Work(actor, f, response))

    def execute1[T1, R](actor: Actor)(f: (T1) => R)(v1: T1)(response: Future[R]) = submit0(actor, Work1(actor, f, v1, response))

    def execute2[T1, T2, R](actor: Actor)(f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]) = submit0(actor, Work2(actor, f, v1, v2, response))

    def execute3[T1, T2, T3, R](actor: Actor)(f: (T1, T2, T3) => R)(v1: T1, v2: T2, v3: T3)(response: Future[R]) = submit0(actor, Work3(actor, f, v1, v2, v3, response))

    def execute4[T1, T2, T3, T4, R](actor: Actor)(f: (T1, T2, T3, T4) => R)(v1: T1, v2: T2, v3: T3, v4: T4)(response: Future[R]) = submit0(actor, Work4(actor, f, v1, v2, v3, v4, response))

    def execute5[T1, T2, T3, T4, T5, R](actor: Actor)(f: (T1, T2, T3, T4, T5) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5)(response: Future[R]) = submit0(actor, Work5(actor, f, v1, v2, v3, v4, v5, response))

    def execute6[T1, T2, T3, T4, T5, T6, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6)(response: Future[R]) = submit0(actor, Work6(actor, f, v1, v2, v3, v4, v5, v6, response))

    def execute7[T1, T2, T3, T4, T5, T6, T7, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7)(response: Future[R]) = submit0(actor, Work7(actor, f, v1, v2, v3, v4, v5, v6, v7, response))

    def execute8[T1, T2, T3, T4, T5, T6, T7, T8, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)(response: Future[R]) = submit0(actor, Work8(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, response))

    def execute9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9)(response: Future[R]) = submit0(actor, Work9(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, response))

    def execute10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10)(response: Future[R]) = submit0(actor, Work10(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, response))

    def execute11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11)(response: Future[R]) = submit0(actor, Work11(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, response))

    def execute12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12)(response: Future[R]) = submit0(actor, Work12(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, response))

    def execute13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13)(response: Future[R]) = submit0(actor, Work13(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, response))

    def execute14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14)(response: Future[R]) = submit0(actor, Work14(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, response))

    def execute15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15)(response: Future[R]) = submit0(actor, Work15(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, response))

    def execute16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16)(response: Future[R]) = submit0(actor, Work16(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, response))

    def execute17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17)(response: Future[R]) = submit0(actor, Work17(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, response))

    def execute18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18)(response: Future[R]) = submit0(actor, Work18(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, response))

    def execute19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19)(response: Future[R]) = submit0(actor, Work19(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, response))

    def execute20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20)(response: Future[R]) = submit0(actor, Work20(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, (v19, v20), response))

    def execute21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21)(response: Future[R]) = submit0(actor, Work21(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, (v19, v20, v21), response))

    def execute22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22)(response: Future[R]) = submit0(actor, Work22(actor, f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, (v19, v20, v21, v22), response))

    def submit0(actor: Actor, strategyTask: StrategyWorkerTask): Unit = {
      var exit = false

      while (!exit) {
        val newWorker = new StrategyWorker(actor, assignments)

        if (assignments.putIfAbsent(actor, newWorker) == null) {
          executorService.execute(newWorker)
        }

        try {
          assignments.get(actor).asInstanceOf[StrategyWorker].offer(strategyTask)

          exit = true
        }
        catch {
          case e: NullPointerException =>
        }
      }
    }
  }

  private[ActorStrategyMultiThreaded] class StrategyWorker(actor: Actor, assignments: ConcurrentMap[AnyRef, StrategyWorker]) extends Runnable{

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
      ActorContext.withActorFn(actor){
        while (!done) {
          val head = queue.poll(1, TimeUnit.MILLISECONDS)

          if (head != null) {
            head.apply()
          }
          else {
            doneLock.writeLock {
              if (queue.size == 0) {
                done = true
                assignments.remove(actor, this)
              }
            }
          }
        }
      }
    }
  }

  object ActorContext{
    private val tl = new ThreadLocal[Actor]

    def get : Option[Actor] = {
      if (tl.get() != null) Some(tl.get()) else None
    }

    def withActorFn(actor: Actor)(f : => Unit) = {
      val old = get
      try {
        tl.set(actor)
        f
      } finally {
        tl.set(old.getOrElse(null))
      }
    }
  }

  private[ActorStrategyMultiThreaded] class FutureDeliveryStrategyWorker[R](actor: Actor) extends FutureDeliveryStrategy{
   def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) {
     actorExecutionStrategy.submit0(actor, Deliver(value, listeners, errorHandler))
   }
  }

  private[ActorStrategyMultiThreaded] sealed trait StrategyWorkerTask extends Function0[Unit]
  private[ActorStrategyMultiThreaded] case class Work[R](actor: Actor, f: () => R, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute(actor)(f)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work1[T1, R](actor: Actor, f: T1 => R, v1: T1, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute1(actor)(f)(v1)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work2[T1, T2, R](actor: Actor, f: (T1, T2) => R, v1: T1, v2: T2, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute2(actor)(f)(v1, v2)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work3[T1, T2, T3, R](actor: Actor, f: (T1, T2, T3) => R, v1: T1, v2: T2, v3: T3, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute3(actor)(f)(v1, v2, v3)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work4[T1, T2, T3, T4, R](actor: Actor, f: (T1, T2, T3, T4) => R, v1: T1, v2: T2, v3: T3, v4: T4, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute4(actor)(f)(v1, v2, v3, v4)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work5[T1, T2, T3, T4, T5, R](actor: Actor, f: (T1, T2, T3, T4, T5) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute5(actor)(f)(v1, v2, v3, v4, v5)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work6[T1, T2, T3, T4, T5, T6, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute6(actor)(f)(v1, v2, v3, v4, v5, v6)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work7[T1, T2, T3, T4, T5, T6, T7, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute7(actor)(f)(v1, v2, v3, v4, v5, v6, v7)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work8[T1, T2, T3, T4, T5, T6, T7, T8, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute8(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute9(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute10(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute11(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute12(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute13(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute14(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute15(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute16(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute17(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute18(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute19(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19_20: (T19, T20), response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute20(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19_20._1, v19_20._2)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19_20_21: (T19, T20, T21), response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute21(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19_20_21._1, v19_20_21._2, v19_20_21._3)(response)
    }
  }
  private[ActorStrategyMultiThreaded] case class Work22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](actor: Actor, f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19_20_21_22: (T19, T20, T21, T22), response: Future[R]) extends StrategyWorkerTask{
    def apply() {
      executionSequential.actorExecutionStrategy.execute22(actor)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19_20_21_22._1, v19_20_21_22._2, v19_20_21_22._3, v19_20_21_22._4)(response)
    }
  }

  private[ActorStrategyMultiThreaded] case class Deliver[R](value: R, listeners: Iterable[R => Unit], errorHandler: List[Throwable] => Unit) extends StrategyWorkerTask{
    def apply() {
      futureSequential.futureDeliveryStrategy.deliver(value, listeners, errorHandler)
    }
  }
}

trait ActorExecutionStrategyFixedPool extends ActorStrategyMultiThreaded {
  def actorExecutionStrategyThreadPoolSize: Int

  lazy val executorService = Executors.newFixedThreadPool(actorExecutionStrategyThreadPoolSize)
}


