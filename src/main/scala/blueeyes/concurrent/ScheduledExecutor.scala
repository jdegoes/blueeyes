package blueeyes.concurrent

import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}

trait ScheduledExecutor{
  def scheduledExecutor: ScheduledExecutorService

  def once[A](actor: () => Future[A], time: Long, unit: TimeUnit)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[A] = {
    val future = new Future[A]()

    val executorFuture = scheduledExecutor.schedule(new Runnable{
      def run = {
        val actorFuture = actor()

        actorFuture.deliverTo(a => future.deliver(a))
        actorFuture.ifCanceled(error => future.cancel(error))

        future.ifCanceled(error => actorFuture.cancel(error) )
      }
    }, time, unit)

    future.ifCanceled(error => executorFuture.cancel(true) )

    future
  }

  def forever[A](actor: () => Future[A], time: Long, unit: TimeUnit)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Unit] = {
    val future = new Future[Unit]()

    val executorFuture = scheduledExecutor.scheduleWithFixedDelay(new Runnable{
      def run = {
        val actorFuture = actor()

        future.ifCanceled(error => actorFuture.cancel(error))
      }
    }, time, time, unit)

    future.ifCanceled(error => executorFuture.cancel(true))

    future
  }

  def repeat[A, Z](actor: () => Future[A], time: Long, unit: TimeUnit, times: Int, seed: Z)(fold: (Z, A) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = {
    class Folder{
      private var count = 0

      def countedFold(z: Z, a: A) = {
        count = count + 1
        fold(z, a)
      }
      def pred(z: Z) = count < times
    }

    val folder = new Folder()
    repeatWhile(actor, time, unit, folder.pred _, seed)(folder.countedFold _)
  }

  def repeatWhile[A, Z](actor: () => Future[A], time: Long, unit: TimeUnit, pred: (Z) => Boolean, seed: Z)(fold: (Z, A) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = {
    val future = new Future[Z]()

    if (pred(seed)){
      val executorFuture = scheduledExecutor.scheduleWithFixedDelay(new Runnable{
        private var result   = seed

        def run = {
          if (pred(result)){
            val actorFuture = actor()

            actorFuture.deliverTo(a => result = fold(result, a))

            future.ifCanceled(error => actorFuture.cancel(error))

            if (!pred(result)) future.deliver(result)
          }
        }
      }, time, time, unit)

      future.ifCanceled(error => executorFuture.cancel(true))
      future.deliverTo(z => executorFuture.cancel(true))
    }
    else future.deliver(seed)

    future
  }
}

trait ScheduledExecutorMultiThreaded extends ScheduledExecutor{
  def scheduledExecutor = Executors.newScheduledThreadPool(100)
}

object ScheduledExecutor extends ScheduledExecutorMultiThreaded

case class SchedulableActor[A, B](actor: Actor[A, B]) {
  def !@ (msg: A, time: Long, unit: TimeUnit)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[B] = ScheduledExecutor.once(() => actor ! msg , time, unit)

  def !@~ (msg: A, time: Long, unit: TimeUnit)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Unit] = ScheduledExecutor.forever(() => actor ! msg , time, unit)

  def !@ [Z](msg: A, time: Long, unit: TimeUnit, pred: (Z) => Boolean, seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = ScheduledExecutor.repeatWhile(() => actor ! msg , time, unit, pred, seed)(fold)

  def !@ [Z](msg: A, time: Long, unit: TimeUnit, times: Int, seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = ScheduledExecutor.repeat(() => actor ! msg , time, unit, times, seed)(fold)
}
object ScheduledActor {
  implicit def actorToSchedulableActor[A, B](a: Actor[A, B]): SchedulableActor[A, B] = new SchedulableActor(a)
}