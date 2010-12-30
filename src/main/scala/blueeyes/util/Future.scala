package blueeyes.util

/** A future based on time-stealing rather than threading. Unlike Scala's future, 
 * this future has three possible states: undecided (not done), canceled (aborted
 * due to request or error), or delivered. The triune nature of the future makes
 * it easy to propagate errors through chains of futures -- something not possible 
 * with bi-state futures.
 * <p>
 * A time-stealing future has certain requirements that other futures do not. In 
 * particular, a time-stealing future cannot abort delivery just because a single
 * listener throws an exception, because a thread that delivers a value to a future
 * does not expect to be forcibly terminated due to an error in client code.
 * Thus, all exceptions are "swallowed" -- although they will cancel derived futures,
 * the exceptions will not terminate calling code. All such "swallowed" exceptions
 * are reported to the thread's default exception handler.
 */
class Future[T] {
  import scala.collection.mutable.ArrayBuffer
  
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  
  private val _listeners: ArrayBuffer[T => Unit] = new ArrayBuffer()
  private def listeners: List[T => Unit] = readLock { _listeners.toList }
  
  private var _result: Option[T] = None
  private var _isSet: Boolean = false
  private var _isCanceled: Boolean = false
  
  private val _cancelers: ArrayBuffer[Unit => Boolean] = new ArrayBuffer
  private def cancelers: List[Unit => Boolean] = readLock { _cancelers.toList }
  
  private var _canceled: ArrayBuffer[Option[Throwable] => Unit] = new ArrayBuffer
  private def canceled: List[Option[Throwable] => Unit] = _canceled.toList
  
  private var _error: Option[Throwable] = None

  /** Delivers the value of the future to anyone awaiting it. If the value has
   * already been delivered, this method will throw an exception.
   * <p>
   * If when evaluating the lazy value, an error is thrown, the error will be
   * used to cancel the future.
   */
  def deliver(computation: => T): Future[T] = {
    writeLock {
      if (_isCanceled) this
      else if (_isSet) Predef.error("Future already delivered")
      else {
        try {
          val deliverable = computation
          
          _result = Some(deliverable)
          _isSet  = true

          listeners.foreach { listener =>
            trapError {
              listener(deliverable)
            }
          }

          _listeners.clear()
        }
        catch {
          case error: Throwable => 
            cancel(error)
        }
      }
    }
    
    this
  }

  /** Installs the specified canceler on the future. Under ordinary
   * circumstances, the future will not be canceled unless all cancelers
   * return true. If the future is already done, this method has no effect.
   * <p>
   * This method does not normally need to be called. It's provided primarily
   * for the implementation of future primitives.
   */
  def allowCancelOnlyIf(f: Unit => Boolean): Future[T] = {
    writeLock {
      if (!isDone) _cancelers.append(f)
    }

    this
  }

  /** Installs a handler that will be called if and only if the future is
   * canceled.
   * <p>
   * This method does not normally need to be called, since there is no
   * difference between a future being canceled and a future taking an
   * arbitrarily long amount of time to evaluate. It's provided primarily
   * for implementation of future primitives to save resources when it's
   * explicitly known the result of a future will not be used.
   */
  def ifCanceled(f: Option[Throwable] => Unit): Future[T] = {
    writeLock {
      if (isCanceled) f(_error)
      else if (!isDone) _canceled.append(f)
    }

    this
  }
  
  /** Returns a Future that always succeeds -- if this future is canceled, the
   * specified default value will be delivered to the returned future.
   * <p>
   * <pre>
   * f.orElse(3).map(_ * 5)
   * </pre>
   */
  def orElse(default: => T): Future[T] = {
    val f = new Future[T]
    
    f.allowCancelOnlyIf { _ => false }
    
    this.deliverTo { result =>
      f.deliver(result)
    }
    
    this.ifCanceled { _ =>
      f.deliver(default)
    }
    
    f
  }

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   * <p>
   * If a future is canceled, the result will never be delivered.
   *
   * @return true if the future is canceled, false otherwise.
   */
  def cancel(): Boolean = cancel(None)

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   */
  def cancel(e: Throwable): Boolean = cancel(Some(e))
  
  def cancel(o: Option[Throwable]): Boolean = internalCancel(o)

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

  /** Delivers the result of the future to the specified handler as soon as it
   * is delivered.
   */
  def deliverTo(f: T => Unit): Future[T] = {
    writeLock {
      if (!isCanceled) {
        if (isDelivered) f(_result.get)
        else _listeners.append(f)
      }
      this
    }
  }

  /** Uses the specified function to transform the result of this future into
   * a different value, returning a future of that value.
   * <p]
   * urlLoader.load("image.png").map(data => return new Image(data)).deliverTo(image => imageContainer.add(image))
   */
  def map[S](f: T => S): Future[S] = {
    var fut: Future[S] = new Future

    deliverTo { t =>
      fut.deliver(f(t))
    }

    ifCanceled(fut.forceCancel)

    fut
  }
  
  /** Simply returns the passed in future. Used when the result of a previous 
   * future is not needed.
   */
  def then[S](f: Future[S]): Future[S] = f

  /** Maps the result of this future to another future, and returns a future
   * of the result of that future. Useful when chaining together multiple
   * asynchronous operations that must be completed sequentially.
   * <p>
   * <pre>
   * <code>
   * urlLoader.load("config.xml").flatMap { xml =>
   *   urlLoader.load(parse(xml).mediaUrl)
   * }.deliverTo { loadedMedia =>
   *   container.add(loadedMedia)
   * }
   * </code>
   * </pre>
   */
  def flatMap[S](f: T => Future[S]): Future[S] = {
    var fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t).deliverTo { s =>
          fut.deliver(s)
        }.ifCanceled {
          fut.forceCancel
        }
      }
    }
    
    ifCanceled(fut.forceCancel)

    return fut
  }
  
  def flatMapOption[S](f: T => Option[S]): Future[S] = {
    var fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t) match {
          case None => fut.cancel()
        
          case Some(s) => fut.deliver(s)
        }
      }
    }

    ifCanceled(fut.forceCancel)

    return fut
  }
  
  def flatMapEither[F <: Throwable, S](f: T => Either[F, S]): Future[S] = {
    var fut: Future[S] = new Future

    deliverTo { t =>
      cancelFutureOnError(fut) {
        f(t) match {
          case Left(error) => fut.cancel(error)
        
          case Right(result) => fut.deliver(result)
        }
      }
    }

    ifCanceled(fut.forceCancel)

    return fut
  }

  def flatten[S](implicit witness: T => Future[S]): Future[S] = flatMap(witness)

  /** Returns a new future that will be delivered only if the result of this
   * future is accepted by the specified filter (otherwise, the new future
   * will be canceled).
   */
  def filter(f: T => Boolean): Future[T] = {
    var fut: Future[T] = new Future

    deliverTo { t => 
      cancelFutureOnError(fut) {
        if (f(t)) fut.deliver(t) else fut.forceCancel(None)
      }
    }

    ifCanceled(fut.forceCancel)

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
    var zipped: Future[(T, A)] = new Future

    var f1 = this

    def deliverZip = {
      if (f1.isDelivered && f2.isDelivered) {
        zipped.deliver((f1.value.get, f2.value.get))
      }
    }

    f1.deliverTo(v => deliverZip)
    f2.deliverTo(v => deliverZip)

    zipped.allowCancelOnlyIf { _ => f1.cancel || f2.cancel }

    f1.ifCanceled(zipped.forceCancel)
    f2.ifCanceled(zipped.forceCancel)

    zipped
  }

  /** Retrieves the value of th future, as an option. If the future has nothing
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

  private def forceCancel(error: Option[Throwable]): Future[T] = {
    writeLock {
      if (!_isCanceled) {
        _error      = error        
        _isCanceled = true

        canceled.foreach { listener =>
          trapError {
            listener(error)
          }
        }
        
        _canceled.clear()
        _cancelers.clear()
      }
    }

    return this
  }
  
  private def internalCancel(error: Option[Throwable]): Boolean = {
    writeLock {
      if (isDone) false           // [-- Already done, can't be canceled
      else if (isCanceled) true   // <-- Already canceled, nothing to do
      else {                      // <-- Ask to see if everyone's OK with canceling
        var shouldCancel = cancelers.foldLeft(true){ (v, canceller) => v && canceller()}

        if (shouldCancel) {
          // Everyone's OK with canceling, mark state & notify:
          forceCancel(error)
        }

        shouldCancel
      }
    }
  }
  
  private def readLock[S](f: => S): S = {
    lock.readLock.lock()
    try {
      f
    }
    finally {
      lock.readLock.unlock()
    }
  }
  
  private def writeLock[S](f: => S): S = {
    lock.writeLock.lock()
    try {
      f
    }
    finally {
      lock.writeLock.unlock()
    }
  }
  
  private def cancelFutureOnError[T](future: Future[_])(f: => T): Unit = {
    try {
      f
    }
    catch {
      case error: Throwable => future.cancel(error)
    }
  }
  
  private def trapError[T](f: => T): Unit = {
    try {
      f
    }
    catch { 
      case error: Throwable =>
        Thread.getDefaultUncaughtExceptionHandler match {
          case handler: Thread.UncaughtExceptionHandler =>
            handler.uncaughtException(Thread.currentThread(), error)
            
          case null => 
        }
    }
  }
}

object Future {
  /** Creates a "dead" future that is canceled and will never be delivered.
   */
  def dead[T]: Future[T] = {
    val f = new Future[T]
    f.cancel
    f
  }
  
  def dead[T](e: Throwable): Future[T] = {
    val f = new Future[T]
    f.cancel(e)
    f
  }
  
  def apply[T](t: T): Future[T] = new Future().deliver(t: T)
  
  def apply[T](futures: Future[T]*): Future[List[T]] = {
    import java.util.concurrent.ConcurrentHashMap
    
    val resultsMap = new ConcurrentHashMap[java.lang.Integer, T]
    
    val result = new Future[List[T]]
    
    val remaining = new java.util.concurrent.atomic.AtomicInteger(futures.length)
    
    (0 to futures.length).zip(futures).foreach { pair =>
      val index  = pair._1
      val future = pair._2
      
      future.deliverTo { result =>
        resultsMap.put(index, result)
      }
    }
    
    futures.foreach { future =>
      future.ifCanceled { e =>
        result.cancel(e)
      }
      
      future.deliverTo { _ => 
        val newCount = remaining.decrementAndGet
      
        if (newCount == 0) {
          var list: List[T] = Nil
          
          for (i <- 0 to futures.length) {
            list = resultsMap.get(i) :: list
          }
          
          result.deliver(list)
        }
      }
    }
    
    result
  }
  
  def async[T](f: => T): Future[T] = {
    val result = new Future[T]
    
    import scala.actors.Actor._
    
    actor {
      try {
        result.deliver(f)
      }
      catch {
        case t: Throwable => result.cancel(t)
      }
    }
    
    result
  }
}

trait FutureImplicits {
  implicit def any2Future[T, S >: T](any: T): Future[S] = Future(any: S)
}
object FutureImplicits extends FutureImplicits