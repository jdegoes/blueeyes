package blueeyes.concurrent

import blueeyes.util.metrics.Duration
import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}

trait ScheduledExecutor{

  type ScheduledFunction[A, B] = A => Future[B]

  def scheduledExecutor: ScheduledExecutorService

  def once[B](f: Function0[Future[B]], duration: Duration): Future[B] = once(toScheduledFunction(f), (), duration)

  def once[A, B](f: ScheduledFunction[A, B], message: => A, duration: Duration): Future[B] = {
    val future = new Future[B]()

    val executorFuture = scheduledExecutor.schedule(new Runnable{
      def run = {
        val actorFuture = f(message)

        actorFuture.deliverTo(b => future.deliver(b))
        actorFuture.ifCanceled(error => future.cancel(error))

        future.ifCanceled(error => actorFuture.cancel(error) )
      }
    }, duration.time.toLong, duration.unit)

    future.ifCanceled(error => executorFuture.cancel(true) )

    future
  }

  def forever[B](f: Function0[Future[B]], duration: Duration): Future[Unit] = forever(toScheduledFunction(f), (), duration)

  def forever[A, B](f: ScheduledFunction[A, B], message: => A, duration: Duration): Future[Unit] =
    unfold[A, B, Unit](f, message, duration)(())((z: Unit, b: B) => ((), Some(message)))

  def repeat[B, Z](f: Function0[Future[B]], duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeat(toScheduledFunction(f), (), duration, times)(seed)(fold)

  def repeat[A, B, Z](f: ScheduledFunction[A, B], message: => A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = {
    class Folder{
      private var count = 0

      def countedFold(z: Z, b: B) = {
        count = count + 1
        fold(z, b)
      }
      def pred(z: Z) = count < times
    }

    val folder = new Folder()
    repeatWhile(f, message, duration, folder.pred _)(seed)(folder.countedFold _)
  }

  def repeatWhile[B, Z](f: Function0[Future[B]], duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeatWhile(toScheduledFunction(f), (), duration, pred)(seed)(fold)

  def repeatWhile[A, B, Z](f: ScheduledFunction[A, B], message: => A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z): Future[Z] = {
    if (pred(seed)){
      unfold(f, message, duration)(seed){(z: Z, b: B) =>
        val newZ = fold(z, b)
        (newZ, if (pred(newZ)) Some(message) else None )
      }
    }
    else Future.sync[Z](seed)
  }

  def unfold[A, B, Z](f: A => Future[B], firstMessage: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = {

    val future      = new Future[Z]()
    val start       = System.currentTimeMillis

    def delayed(message: A) = once(f, message, duration)
    def now(message: A)     = f(message)

    def schedule(scheduleActor: A => Future[B], message: A, scheduleSeed: Z, nextSchedule: Long){
      val nextScheduleTime   = System.currentTimeMillis + duration.unit.convert(duration.time.toLong * nextSchedule, TimeUnit.MILLISECONDS)

      val actorFuture    = scheduleActor(message)
      future.ifCanceled(error => actorFuture.cancel(error))
      actorFuture.ifCanceled(error => future.cancel(error))

      actorFuture.deliverTo {b =>
        val (newZ, newA) = generator(scheduleSeed, b)

        newA match{
          case None    if !future.isCanceled    => future.deliver(newZ)
          case Some(x) if !future.isCanceled => {
            val (currentActor, delay) = if (System.currentTimeMillis < nextScheduleTime) (delayed _, 2)
            else (now _, 1)

            schedule(currentActor, x, newZ, nextSchedule + delay)
          }
          case _ =>
        }
      }
    }
    schedule(delayed _, firstMessage, seed, 2)

    future
  }

  def !@[A, B](f: ScheduledFunction[A, B], message: => A, duration: Duration) = once(f, message, duration)

  def !@~[A, B](f: ScheduledFunction[A, B], message: => A, duration: Duration) = forever(f, message, duration)

  def !@ [A, B, Z](f: ScheduledFunction[A, B], msg: A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeatWhile(f, msg , duration, pred)(seed)(fold)

  def !@ [A, B, Z](f: ScheduledFunction[A, B], msg: A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeat(f, msg , duration, times)(seed)(fold)

  def !@ [A, B, Z](f: ScheduledFunction[A, B], msg: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = unfold(f, msg , duration)(seed)(generator)

  private def toScheduledFunction[B](f: Function0[Future[B]]) = { v: Unit => f()}
}

trait ScheduledExecutorSingleThreaded extends ScheduledExecutor{
  lazy val scheduledExecutor = Executors.newSingleThreadScheduledExecutor()
}

object ScheduledExecutorSingleThreaded extends ScheduledExecutorSingleThreaded

trait ScheduledExecutorMultiThreaded extends ScheduledExecutor{
  lazy val scheduledExecutor = Executors.newScheduledThreadPool(2)
}

object ScheduledExecutor extends ScheduledExecutorMultiThreaded
