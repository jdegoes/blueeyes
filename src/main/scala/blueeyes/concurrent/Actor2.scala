package blueeyes.concurrent

trait ActorStrategy{
  implicit def actorExecutionStrategy: ActorExecutionStrategy2

  implicit def futureDeliveryStrategy: FutureDeliveryStrategy
}

trait Actor2 extends ActorStrategy{

  def lift1[T1, R](f: T1 => R): T1 => Future[R] = (v1: T1) => lift(actorExecutionStrategy.execute1(f)(v1) _)

  def lift2[T1, T2, R](f: (T1, T2) => R): (T1, T2) => Future[R] = (v1: T1, v2: T2) => lift(actorExecutionStrategy.execute2(f)(v1, v2) _)

  private def lift[R](f: Future[R] => Unit): Future[R] = {
    val response = new Future[R]
    f(response)
    response
  }
}

trait ActorExecutionStrategy2{
  def execute1[T1, R](f: T1 => R)(v1: T1)(response: Future[R]): Unit

  def execute2[T1, T2, R](f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]): Unit
}
