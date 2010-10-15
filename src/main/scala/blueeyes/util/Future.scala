package blueeyes.util

class Future[T] {
  import scala.collection.mutable.ArrayBuffer
  
  var _listeners: ArrayBuffer[T => Unit] = new ArrayBuffer()
  var _result: T = null
  var _isSet: Bool = false
  var _isCanceled: Bool = false
  var _cancelers: ArrayBuffer[Unit => Bool] = new ArrayBuffer()
  var _canceled: ArrayBuffer[Unit => Unit] = new ArrayBuffer()

  /** Delivers the value of the future to anyone awaiting it. If the value has
   * already been delivered, this method will throw an exception.
   */
  def deliver(t: T): Future[T] = {
    if (_isCanceled) this
    else if (_isSet) error("Future already delivered")
    else {
      _result = t
      _isSet  = true

      for (l in _listeners) l(_result)

      _listeners = []

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
  def allowCancelOnlyIf(f: Unit => Bool): Future[T] = {
    if (!isDone()) _cancelers.push(f)

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
    if (isCanceled()) f()
    else if (!isDone()) _canceled.push(f)

    this
  }

  /** Attempts to cancel the future. This may succeed only if the future is
   * not already delivered, and if all cancel conditions are satisfied.
   * [p]
   * If a future is canceled, the result will never be delivered.
   *
   * @return true if the future is canceled, false otherwise.
   */
  def cancel(): Bool = {
    if (isDone()) false   // [-- Already done, can't be canceled
    else if (isCanceled()) true  // <-- Already canceled, nothing to do
    else {                        // <-- Ask to see if everyone's OK with canceling
      var r = true

      for (canceller in _cancelers) r = r && canceller()

      if (r) {
        // Everyone's OK with canceling, mark state & notify:
        forceCancel()
      }

      r
    }
  }

  /** Determines if the future is "done" -- that is, delivered or canceled.
   */
  def isDone(): Bool = {
    isDelivered() || isCanceled()
  }

  /** Determines if the future is delivered.
   */
  def isDelivered(): Bool = _isSet

  /** Determines if the future is canceled.
   */
  def isCanceled(): Bool = _isCanceled

  /** Delivers the result of the future to the specified handler as soon as it
   * is delivered.
   */
  def deliverTo(f: T => Unit): Future[T] = {
    if (!isCanceled()) {
      else if (isDelivered()) f(_result)
      else _listeners.push(f)
    }

    this
  }

  /** Uses the specified function to transform the result of this future into
   * a different value, returning a future of that value.
   * <p]
   * urlLoader.load("image.png").map(data => return new Image(data)).deliverTo(image => imageContainer.add(image))
   */
  def map[S](f: T => S): Future[S] = {
    var fut: Future[S] = new Future()

    deliverTo(t => fut.deliver(f(t)))
    ifCanceled(fut.forceCancel)

    fut
  }
  
  /** Simply returns the passed in future. Used when the result of a previous 
   * future is not needed.
   */
  def then[S](f: Future[S]): Future[S] {
    return f
  }

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
  def flatMap[S](f: T => Future[S]): Future[S] {
    var fut: Future[S] = new Future()

    deliverTo { t =>
      f(t).deliverTo(s: S => {
        fut.deliver(s)
      }).ifCanceled(function() {
        fut.forceCancel()
      })
    }

    ifCanceled(function() { fut.forceCancel() })

    return fut
  }

  /** Returns a new future that will be delivered only if the result of this
   * future is accepted by the specified filter (otherwise, the new future
   * will be canceled).
   */
  def filter(f: T => Bool): Future[T] {
    var fut: Future[T] = new Future()

    deliverTo(t: T => { if (f(t)) fut.deliver(t) else fut.forceCancel() })

    ifCanceled(function() fut.forceCancel())

    return fut
  }

  /** Zips this future and the specified future into another future, whose
   * result is a tuple of the individual results of the futures. Useful when
   * an operation requires the result of two futures, but each future may
   * execute independently of the other.
   */
  def zip[A](f2: Future[A]): Future[(T, A)]> {
    var zipped: Future[(T, A)] = new Future()

    var f1 = this

    def deliverZip = {
      if (f1.isDelivered() && f2.isDelivered()) {
        zipped.deliver(
          (f1.value().get(), f2.value().get())
        )
      }
    }

    f1.deliverTo(v => deliverZip)
    f2.deliverTo(v => deliverZip)

    zipped.allowCancelOnlyIf(function() return f1.cancel() || f2.cancel())

    f1.ifCanceled(function() zipped.forceCancel())
    f2.ifCanceled(function() zipped.forceCancel())

    return zipped
  }

  /** Retrieves the value of the future, as an option.
   */
  def value(): Option[T] {
    return if (_isSet) Some(_result) else None
  }

  def toOption(): Option[T] {
    return value()
  }

  def toList(): List[T] {
    return value().toList()
  }

  private function forceCancel(): Future[T] {
    if (!_isCanceled) {
      _isCanceled = true

      for (canceled in _canceled) canceled()
    }

    return this
  }

  public static function create[T](): Future[T] {
    return new Future[T]()
  }
}

object Future {

  /** Creates a "dead" future that is canceled and will never be delivered.
   */
  def Dead[T]: Future[T] {
    return new Future().withEffect { future =>
      future.cancel()
    }
  }
}