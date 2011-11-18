package blueeyes.concurrent

import akka.dispatch.{Future => AkkaFuture, MessageDispatcher, Dispatchers}
import akka.actor.Scheduler
import akka.util.Duration
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scalaz.{Validation, Success, Failure, Semigroup, Monad, Functor, Apply, Pointed, Applicative}
import scalaz.Scalaz._

/** A future based on time-stealing rather than threading. Unlike Scala's future,
 * this future has three possible states: undecided (not done), canceled (aborted
 * due to request or error), or delivered. The triune nature of the future makes
 * it easy to propagate errors through chains of futures -- something not possible
 * with bi-state futures.
 *
 * A time-stealing future has certain requirements that other futures do not. In
 * particular, a time-stealing future cannot abort delivery just because a single
 * listener throws an exception, because a thread that delivers a value to a future
 * does not expect to be forcibly terminated due to an error in client code.
 * Thus, all exceptions are "swallowed" -- although they will cancel derived futures,
 * the exceptions will not terminate calling code. All such "swallowed" exceptions
 * are reported to the thread's default exception handler.
 */
sealed class Future[T] { self =>
  import scala.collection.mutable.ArrayBuffer

  private val lock = new ReadWriteLock{}

  private val _errorHandlers: ArrayBuffer[List[Throwable] => Unit] = new ArrayBuffer()
  private def errorHandlers: List[List[Throwable] => Unit] = lock.readLock { _errorHandlers.toList }
  private val _listeners: ArrayBuffer[T => Unit] = new ArrayBuffer()
  private def listeners: List[T => Unit] = lock.readLock { _listeners.toList }

  @volatile private var _result: Option[T] = None
  @volatile private var _isSet: Boolean = false
  @volatile private var _isCanceled: Boolean = false

  @volatile private var _canceled: ArrayBuffer[Option[Throwable] => Unit] = new ArrayBuffer
  private def canceled: List[Option[Throwable] => Unit] = _canceled.toList

  @volatile private var _error: Option[Throwable] = None

  /** Delivers the value of the future to anyone awaiting it. If the value has
   * already been delivered, this method will throw an exception.
   *
   * If when evaluating the lazy value, an error is thrown, the error will be
   * used to cancel the future.
   */
  def deliver(computation: => T): Future[T] = {
    lock.writeLock {
      if (_isSet) sys.error("Future already delivered")
      else if (_isCanceled) None
      else {
        try {
          val deliverable = computation // This line could throw an exception since it's the first time we evaluate the computaiton

          // No exception was thrown, so future can be marked as delivered:
          _result = Some(deliverable)
          _isSet  = true

          Some(Right(deliverable))
        }
        catch {
          case error: Throwable =>
            Some(Left(error))
        }
      }
    } match {
      case None =>

      case Some(Left(why)) => cancel(why)

      case Some(Right(deliverable)) =>
        // This is actually safe despite the absence of a write lock,
        // because the future's state is set to delivered, so _listeners
        // list cannot possibly change except through this method.

        deliverAndHandleError(_result.get, listeners, errorHandlers)

        _listeners.clear()
    }

    this
  }

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   *
   * If a future is canceled, the result will never be delivered.
   *
   * @return true if the future is canceled, false otherwise.
   */
  def cancel(): Boolean = cancel(None)

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   */
  def cancel(e: Throwable): Boolean = cancel(Some(e))

  def cancel(o: Option[Throwable]): Boolean = cancel(o, !isDone)

  protected def cancel(error: Option[Throwable], iff: => Boolean): Boolean = {
    def setError: Boolean = {
      _error      = error
      _isCanceled = true
      true
    }

    if (lock.writeLock(iff && !_isCanceled && setError)) {
      deliverAndHandleError(_error, _canceled.toList, errorHandlers)
      _canceled.clear()
      true
    } else {
      false
    }
  }

  /** Installs a handler that will be called if and only if the future is
   * canceled.
   *
   * This method does not normally need to be called, since there is no
   * difference between a future being canceled and a future taking an
   * arbitrarily long amount of time to complete. It's provided primarily
   * for implementation of future primitives to save resources when it's
   * explicitly known the result of a future will not be used.
   */
  def ifCanceled(f: Option[Throwable] => Unit): Future[T] = {
    lock.writeLock {
      if (isCanceled) deliverAndHandleError(_error, f :: Nil, errorHandlers)
      else if (!isDone) _canceled.append(f)
    }

    this
  }

  /** Converts the future to a future of unit, which delivers no information.
   */
  def toUnit: Future[Unit] = this.map(_ => ())

  /** Returns a Future that always succeeds -- if this future is canceled, the
   * specified default value will be delivered to the returned future.
   *
   * If a client attempts to cancel the returned future, the cancel request will
   * be propagated to the original future, but has no effect on the returned
   * future, which is always guaranteed to deliver.
   *
   * {{{
   * f.orElse(3).map(_ * 5)
   * }}}
   */
  def orElse(default: => T): Future[T] = orElse { why: Option[Throwable] => default  }

  /** Returns a Future that always succeeds -- if this future is canceled, the
   * specified function will be used to create the value that will be delivered
   * to the returned future. Useful when the default value depends on the nature
   * of the error that canceled the future.
   *
   * {{{
   * f.orElse(why => 3).map(_ * 5)
   * }}}
   */
  def orElse(defaultFactory: Option[Throwable] => T): Future[T] = {
    lazy val f = new Future[T] {
      def fatal(why: Option[Throwable]) = {
        super.cancel(why, true)
      }

      override def cancel(why: Option[Throwable]): Boolean = {
        self.cancel(why)
        false
      }

      override protected def cancel(why: Option[Throwable], iff: => Boolean) = self.cancel(why, iff)
    }

    self.deliverTo { result =>
      f.deliver(result)
    }

    self.ifCanceled { why =>
      trapError(defaultFactory(why)) match {
        case Failure(why)     => f.fatal(Some(why))
        case Success(result) => f.deliver(result)
      }
    }

    f
  }

  /** Determines if the future is "done" -- that is, delivered or canceled.
   */
  def isDone: Boolean = {
    isDelivered || isCanceled
  }

  /** Determines if the future is delivered.
   */
  def isDelivered: Boolean = _isSet

  /** Determines if the future is canceled.
   */
  def isCanceled: Boolean = _isCanceled

  /** Splits a future of a tuple into a tuple of futures.
   */
  def split[U, V](implicit witness: T => (U, V)): (Future[U], Future[V]) = {
    val actual = map(witness)

    (actual.map(_._1), actual.map(_._2))
  }

  /** Alias for zip.
   */
  def join[S](f: Future[S]): Future[(T, S)] = zip(f)

  /** Delivers the result of the future to the specified handler as soon as it
   * is delivered.
   */
  def deliverTo(f: T => Unit): Future[T] = {
    lock.writeLock {
      if (!isCanceled) {
        if (isDelivered) deliverAndHandleError(_result.get, f :: Nil, errorHandlers)
        else _listeners.append(f)
      }
      this
    }
  }

  /** Delivers listeners errors to the specified handler as soon as they
   * are happened.
  */
  def trap(errorHandler: List[Throwable] => Unit){
    lock.writeLock {
      _errorHandlers.append(errorHandler)
    }
  }

  /** Uses the specified function to transform the result of this future into
   * a different value, returning a future of that value.
   *
   * urlLoader.load("image.png").map(data => new Image(data)).deliverTo(image => imageContainer.add(image))
   */
  def map[S](f: T => S): Future[S] = {
    val fut: Future[S] = new Future

    deliverTo { t =>
      fut.deliver(f(t))
    }

    ifCanceled { why =>
      fut.cancel(why, true)
    }

    fut
  }

  /** Simply returns the passed in future. Used when the result of a previous
   * future is not needed.
   */
  def then[S](f: Future[S]): Future[S] = flatMap(_ => f)

  /** Maps the result of this future to another future, and returns a future
   * of the result of that future. Useful when chaining together multiple
   * asynchronous operations that must be completed sequentially.
   *
   * {{{
   * urlLoader.load("config.xml").flatMap { xml =>
   *   urlLoader.load(parse(xml).mediaUrl)
   * }.deliverTo { loadedMedia =>
   *   container.add(loadedMedia)
   * }
   * }}}
   */
  def flatMap[S](f: T => Future[S]): Future[S] = {
    val fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t).deliverTo { s =>
          fut.deliver(s)
        }.ifCanceled {
          fut.cancel(_, true)
        }
      }
    }

    ifCanceled(fut.cancel(_, true))

    fut
  }

  def flatMapOption[S](f: T => Option[S]): Future[S] = {
    val fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t) match {
          case None => fut.cancel()

          case Some(s) => fut.deliver(s)
        }
      }
    }

    ifCanceled(fut.cancel(_, true))

    fut
  }

  def flatMapEither[F <: Throwable, S](f: T => Either[F, S]): Future[S] = {
    val fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t) match {
          case Left(error) => fut.cancel(error)

          case Right(result) => fut.deliver(result)
        }
      }
    }

    ifCanceled(fut.cancel(_, true))

    fut
  }

  def flatten[S](implicit witness: T => Future[S]): Future[S] = flatMap(witness)

  /** Returns a new future that will be delivered only if the result of this
   * future is accepted by the specified filter (otherwise, the new future
   * will be canceled without cause).
   */
  def filter(f: T => Boolean): Future[T] = {
    val fut: Future[T] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        if (f(t)) fut.deliver(t) else fut.cancel(None, true)
      }
    }

    ifCanceled(fut.cancel(_, true))

    fut
  }

  /** Delivers the result of the future to the specified callback.
   */
  def foreach(f: T => Unit): Unit = deliverTo(f)

  /** Zips this future and the specified future into another future, whose
   * result is a tuple of the individual results of the futures. Useful when
   * an operation requires the result of two futures, but each future may
   * execute independently of the other.
   */
  def zip[A](f2: Future[A]): Future[(T, A)] = {
    val f1 = this

    class ZippedFuture extends Future[(T, A)] {
      override def cancel(why: Option[Throwable]): Boolean = {
        f1.cancel(why) || f2.cancel(why)
      }
    }

    val zipped = new ZippedFuture()

    def deliverZip = {
      if (f1.isDelivered && f2.isDelivered && !zipped.isDone) {
        try {
          zipped.deliver((f1.value.get, f2.value.get))
        }
        catch {
          // Due to unlikely race condition, it's possible we'll try to
          // deliver more than once.
          case _ =>
        }
      }
    }

    f1.deliverTo(v => deliverZip)
    f2.deliverTo(v => deliverZip)

    f1.ifCanceled(zipped.cancel(_, true))
    f2.ifCanceled(zipped.cancel(_, true))

    zipped
  }

  /** Retrieves the value of the future, as an option. If the future has nothing
   * been delivered, or produced an error, this method will return None.
   */
  def value: Option[T] = _result

  /** Retrieves the error of the future, as an option. If the future has been
   * delivered, or has not produced an error, this method will return None.
   */
  def error: Option[Throwable] = _error

  def toOption: Option[T] = value

  def toList: List[T] = value.toList

  /** Converts the Future to an Either. This function may only be called if the
   * future is done.
   */
  def toEither: Either[Throwable, T] = if (isCanceled) Left(error.get) else Right(value.get)


  private def cancelFutureOnError[T](future: Future[_])(f: => T): Unit = {
    try f
    catch {
      case error: Throwable => future.cancel(error)
    }
  }

  private def trapError[T](f: => T): Validation[Throwable, T] = {
    try Success(f)
    catch {
      case error: Throwable => 
        handleErrors(errorHandlers)(error :: Nil)
        Failure(error)
    }
  }

  private def deliverAndHandleError[A](value: A, listeners: Iterable[A => Unit], errorHandlers: List[List[Throwable] => Unit]){
    deliver(value, listeners, handleErrors(errorHandlers) _)
  }

  private def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) = {
    val buffer = new ListBuffer[Throwable]

    listeners foreach { listener =>
      try listener(value)
      catch {
        case t: Throwable => buffer += t
      }
    }

    if (buffer.length > 0) errorHandler(buffer.toList)
  }

  private def handleErrors(errorHandlers: List[List[Throwable] => Unit])(errors: List[Throwable]){

    val actualHandlers = errorHandlers match {
      case x :: xs => errorHandlers
      case Nil => List[List[Throwable] => Unit]({errors: List[Throwable] =>
        Thread.getDefaultUncaughtExceptionHandler match {
          case handler: Thread.UncaughtExceptionHandler =>
            val currentThread = Thread.currentThread
            errors foreach { error =>
              handler.uncaughtException(currentThread, error)
            }

          case null =>
        }
      })
    }
    actualHandlers foreach {handler => handler(errors)}
  }
}

trait FutureImplicits {
  implicit def tupleOfFuturesToJoiner[U, V](tuple: (Future[U], Future[V])) = new TupleOfFuturesJoiner(tuple)
  class TupleOfFuturesJoiner[U, V](tuple: (Future[U], Future[V])){
    def join: Future[(U, V)] = tuple._1.zip(tuple._2)
  }

  implicit def fromAkka[T](akkaFuture: AkkaFuture[T]) = new AkkaFutureConversion(akkaFuture)
  class AkkaFutureConversion[T](akkaFuture: AkkaFuture[T]) { 
    def toBlueEyes: Future[T] = {
      val future = new Future[T]()
      akkaFuture.onComplete { value: AkkaFuture[T] =>
        value.value match {
          case Some(Right(value)) => future.deliver(value)
          case Some(Left(error)) => future.cancel(error)
          case None => future.cancel(new RuntimeException("Akka Future has Neither result nor error."))
        }
      }

      future
    }
  }

  implicit def anyToFutureLifted[T](v: => T) = new FutureLifted(v)
  class FutureLifted[T](v: => T) {
    def future: Future[T] = Future.sync(v)
  }
}

object Future extends FutureImplicits {
  /** Creates a "dead" future that is canceled and will never be delivered.
   */
  def dead[T]: Future[T] = new Future[T] ->- (_.cancel)

  def dead[T](e: Throwable): Future[T] = new Future[T] ->- (_.cancel(e))

  def sync[T](t: => T): Future[T] = new Future[T] ->- {
    future => try { future.deliver(t) } catch { case ex => future.cancel(ex) }
  }

  def apply[T](futures: Future[T]*): Future[List[T]] = {
    import java.util.concurrent.ConcurrentHashMap

    val resultsMap = new ConcurrentHashMap[Int, T]
    val result = new Future[List[T]]

    if (!futures.isEmpty){
      val remaining = new java.util.concurrent.atomic.AtomicInteger(futures.length)

      futures.zipWithIndex.foreach {
        case (future, index) => future.ifCanceled { e =>
          result.cancel(e)
        }

        future.deliverTo { futureResult =>
          resultsMap.put(index, futureResult)

          val newCount = remaining.decrementAndGet

          if (newCount == 0) {
            var list: List[T] = Nil

            for (i <- 0 until futures.length) {
              list = resultsMap.get(i) :: list
            }

            result.deliver(list.reverse)
          }
        }
      }
    } else {
      result.deliver(Nil)
    }

    result
  }

  def async[T](t: => T)(implicit futureDispatch: FutureDispatch): Future[T] = {
    akka.dispatch.Future(t, futureDispatch.timeout)(futureDispatch.dispatcher).toBlueEyes
  }

  trait FuturePointed extends Pointed[Future] {
    def pure[A](a: => A) = async(a)
  }

  trait FutureApply extends Apply[Future] {
    override def apply[A, B](f: Future[A ⇒ B], a: Future[A]): Future[B] = f.flatMap(a.map)
  }

  implicit val FutureMonad: Monad[Future] = new Monad[Future] with FuturePointed {
    def bind[A, B](a: Future[A], f: A => Future[B]) = a.flatMap(f) 
  }

  implicit def FutureSemigroup[T: Semigroup]: Semigroup[Future[T]] = new Semigroup[Future[T]] {
    override def append(f1: Future[T], f2: => Future[T]) = f1.zip(f2).map(_.fold(implicitly[Semigroup[T]].append(_, _)))
  }

  implicit val FutureFunctor: Functor[Future] = new Functor[Future] {
    def fmap[A, B](future: Future[A], f: A => B): Future[B] = future.map(f)
  }

  //implicit val FutureApplicative: Applicative[Future] = new Applicative[Future] with FuturePointed with FutureApply

  /*implicit val FutureTraversable: Traversable[Future] = new Traversable[Future] {
    // Cannot implement without blocking
  }*/
}

class FutureDispatch(val timeout: Long, val dispatcher: MessageDispatcher)
object FutureDispatch {
  implicit val DefaultFutureDispatch: FutureDispatch = new FutureDispatch (
    Long.MaxValue, //don't time things out -- TODO: Fix this, extremely dangerous!
    Dispatchers.newExecutorBasedEventDrivenDispatcher("blueeyes_async")
      .withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity.setCorePoolSize(8)
      .setMaxPoolSize(100).setKeepAliveTime(Duration(30, TimeUnit.SECONDS)).build
  )
}
