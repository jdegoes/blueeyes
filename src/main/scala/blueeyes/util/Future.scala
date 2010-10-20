package blueeyes.util

class Future[T] {
  import scala.collection.mutable.ArrayBuffer
  
  var _listeners: ArrayBuffer[T => Unit] = new ArrayBuffer()
  var _result: Option[T] = None
  var _isSet: Boolean = false
  var _isCanceled: Boolean = false
  var _cancelers: ArrayBuffer[Unit => Boolean] = new ArrayBuffer
  var _canceled: ArrayBuffer[Unit => Unit] = new ArrayBuffer

  /** Delivers the value of the future to anyone awaiting it. If the value has
   * already been delivered, this method will throw an exception.
   */
  def deliver(t: T): Future[T] = {
    if (_isCanceled) this
    else if (_isSet) error("Future already delivered")
    else {
      _result = Some(t)
      _isSet  = true

      _listeners.foreach(l => l(t))

      _listeners = new ArrayBuffer

      this
    }
  }

  /** Installs the specified canceler on the future. Under ordinary
   * circumstances, the future will not be canceled unless all cancelers
   * return true. If the future is already done, this method has no effect.
   * [p]
   * This method does not normally need to be called. It's provided primarily
   * for the implementation of future primitives.
   */
  def allowCancelOnlyIf(f: Unit => Boolean): Future[T] = {
    if (!isDone) _cancelers.append(f)

    this
  }

  /** Installs a handler that will be called if and only if the future is
   * canceled.
   * [p]
   * This method does not normally need to be called, since there is no
   * difference between a future being canceled and a future taking an
   * arbitrarily long amount of time to evaluate. It's provided primarily
   * for implementation of future primitives to save resources when it's
   * explicitly known the result of a future will not be used.
   */
  def ifCanceled(f: Unit => Unit): Future[T] = {
    if (isCanceled) f()
    else if (!isDone) _canceled.append(f)

    this
  }

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   * [p]
   * If a future is canceled, the result will never be delivered.
   *
   * @return true if the future is canceled, false otherwise.
   */
  def cancel(): Boolean = {
    if (isDone) false           // [-- Already done, can't be canceled
    else if (isCanceled) true   // <-- Already canceled, nothing to do
    else {                      // <-- Ask to see if everyone's OK with canceling

      var r = _cancelers.foldLeft(true){ (v, canceller) => v && canceller()}

      if (r) {
        // Everyone's OK with canceling, mark state & notify:
        forceCancel()
      }

      r
    }
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

  /** Delivers the result of the future to the specified handler as soon as it
   * is delivered.
   */
  def deliverTo(f: T => Unit): Future[T] = {
    if (!isCanceled) {
      if (isDelivered) f(_result.get)
      else _listeners.append(f)
    }
    this
  }

  /** Uses the specified function to transform the result of this future into
   * a different value, returning a future of that value.
   * <p]
   * urlLoader.load("image.png").map(data => return new Image(data)).deliverTo(image => imageContainer.add(image))
   */
  def map[S](f: T => S): Future[S] = {
    var fut: Future[S] = new Future;

    deliverTo((t: T) => fut.deliver(f(t)));
    ifCanceled((t: Unit) => fut.forceCancel);

    fut;
  }
  
  /** Simply returns the passed in future. Used when the result of a previous 
   * future is not needed.
   */
  def then[S](f: Future[S]): Future[S] = f

  /** Maps the result of this future to another future, and returns a future
   * of the result of that future. Useful when chaining together multiple
   * asynchronous operations that must be completed sequentially.
   * [p]
   * [pre]
   * [code]
   * urlLoader.load("config.xml").flatMap(xml =>
   *   urlLoader.load(parse(xml).mediaUrl)
   * ).deliverTo(loadedMedia =>
   *   container.add(loadedMedia)
   * )
   * [/code]
   * [/pre]
   */
  def flatMap[S](f: T => Future[S]): Future[S] = {
    var fut: Future[S] = new Future

    deliverTo { t =>
      f(t).deliverTo(s => {
        fut.deliver(s)
      }).ifCanceled((t: Unit) => fut.forceCancel())
    }

    ifCanceled((t: Unit) => fut.forceCancel())

    return fut
  }

  def flatten[S](implicit witness: T => Future[S]): Future[S] = flatMap(witness)

  /** Returns a new future that will be delivered only if the result of this
   * future is accepted by the specified filter (otherwise, the new future
   * will be canceled).
   */
  def filter(f: T => Boolean): Future[T] = {
    var fut: Future[T] = new Future

    deliverTo(t => { if (f(t)) fut.deliver(t) else fut.forceCancel() })

    ifCanceled((t: Unit) => fut.forceCancel())

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
        zipped.deliver(
          (f1.value.get, f2.value.get)
        )
      }
    }

    f1.deliverTo(v => deliverZip)
    f2.deliverTo(v => deliverZip)

    zipped.allowCancelOnlyIf((t: Unit) => {f1.cancel || f2.cancel})

    f1.ifCanceled((t: Unit) => zipped.forceCancel)
    f2.ifCanceled((t: Unit) => zipped.forceCancel)

    zipped
  }

  /** Retrieves the value of the future, as an option.
   */
  def value: Option[T] = {
    _result
  }

  def toOption: Option[T] = {
    value
  }

  def toList: List[T] = {
    value.toList
  }

  private def forceCancel(): Future[T] = {
    if (!_isCanceled) {
      _isCanceled = true

      _canceled.foreach(_())
    }

    return this
  }

  def withEffect(f: Future[T] => Unit): Future[T] = {
    f(this);

    this;
  }

}

object Future {
  /** Creates a "dead" future that is canceled and will never be delivered.
   */
  def Dead[T]: Future[T] = {
    val f = new Future[T]
    f.cancel
    f
  }
}

trait FutureImplicits {
  implicit def any2Future[T, S >: T](any: T): Future[S] = new Future[S].deliver(any: T)
}
object FutureImplicits extends FutureImplicits