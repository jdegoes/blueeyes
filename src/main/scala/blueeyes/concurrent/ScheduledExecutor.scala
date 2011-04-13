package blueeyes.concurrent

import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}

trait ScheduledExecutor{

  type ActorType[A, B] = A => Future[B]

  def scheduledExecutor: ScheduledExecutorService

  def once[A, B](actor: ActorType[A, B], message: => A, duration: Duration)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[B] = {
    val future = new Future[B]()

    val executorFuture = scheduledExecutor.schedule(new Runnable{
      def run = {
        val actorFuture = actor(message)

        actorFuture.deliverTo(b => future.deliver(b))
        actorFuture.ifCanceled(error => future.cancel(error))

        future.ifCanceled(error => actorFuture.cancel(error) )
      }
    }, duration.time, duration.unit)

    future.ifCanceled(error => executorFuture.cancel(true) )

    future
  }

  def forever[A, B](actor: ActorType[A, B], message: => A, duration: Duration)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Unit] = {
    val future = new Future[Unit]()

    val executorFuture = scheduledExecutor.scheduleWithFixedDelay(new Runnable{
      def run = {
        val actorFuture = actor(message)

        future.ifCanceled(error => actorFuture.cancel(error))
      }
    }, duration.time, duration.time, duration.unit)

    future.ifCanceled(error => executorFuture.cancel(true))

    future
  }

  def repeat[A, B, Z](actor: ActorType[A, B], message: => A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = {
    class Folder{
      private var count = 0

      def countedFold(z: Z, b: B) = {
        count = count + 1
        fold(z, b)
      }
      def pred(z: Z) = count < times
    }

    val folder = new Folder()
    repeatWhile(actor, message, duration, folder.pred _)(seed)(folder.countedFold _)
  }

  def repeatWhile[A, B, Z](actor: ActorType[A, B], message: => A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = {
    if (pred(seed)){
      unfold(actor, message, duration)(seed){(z: Z, b: B) =>
        val newZ = fold(z, b)
        (newZ, if (pred(newZ)) Some(message) else None )
      }
    }
    else Future.lift[Z](seed)
  }

  def unfold[A, B, Z](actor: A => Future[B], firstMessage: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A]))(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = {

    val future      = new Future[Z]()

    val normalDelay = duration
    val doubleDelay = Duration(duration.time * 2, duration.unit)

    def delayed(message: A) = once(actor, message, duration)
    def now(message: A)     = actor(message)

    def schedule(scheduleActor: A => Future[B], message: A, scheduleSeed: Z, scheduleDelay: Duration){
      val nextSchedule   = System.currentTimeMillis + scheduleDelay.unit.convert(scheduleDelay.time, TimeUnit.MILLISECONDS)

      val actorFuture    = scheduleActor(message)
      future.ifCanceled(error => actorFuture.cancel(error))
      actorFuture.ifCanceled(error => future.cancel(error))

      actorFuture.deliverTo {b =>
        val (newZ, newA) = generator(scheduleSeed, b)

        newA match{
          case None    if !future.isCanceled    => future.deliver(newZ)
          case Some(x) if !future.isCanceled => {
            val (currentActor, delay) = if (System.currentTimeMillis < nextSchedule) (delayed _, doubleDelay)
            else (now _, normalDelay)

            schedule(currentActor, x, newZ, delay)
          }
          case _ =>
        }
      }
    }
    schedule(delayed _, firstMessage, seed, doubleDelay)

    future
  }
}

trait ScheduledExecutorMultiThreaded extends ScheduledExecutor{
  def scheduledExecutor = Executors.newScheduledThreadPool(100)
}

object ScheduledExecutor extends ScheduledExecutorMultiThreaded

case class SchedulableActor[A, B](actor: Actor[A, B]) {
  def !@ (msg: A, duration: Duration)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[B] = ScheduledExecutor.once(actor.! _, msg , duration)

  def !@~ (msg: A, duration: Duration)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Unit] = ScheduledExecutor.forever(actor.! _, msg , duration)

  def !@ [Z](msg: A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = ScheduledExecutor.repeatWhile(actor.! _, msg , duration, pred)(seed)(fold)

  def !@ [Z](msg: A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = ScheduledExecutor.repeat(actor.! _, msg , duration, times)(seed)(fold)

  def !@ [Z](msg: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A]))(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Z] = ScheduledExecutor.unfold(actor.! _, msg , duration)(seed)(generator)
}

object ScheduledActor {
  implicit def actorToSchedulableActor[A, B](a: Actor[A, B]): SchedulableActor[A, B] = new SchedulableActor(a)
}
