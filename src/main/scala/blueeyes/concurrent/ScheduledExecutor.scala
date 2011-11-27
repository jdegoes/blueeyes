package blueeyes.concurrent

import blueeyes.util.metrics.Duration
import blueeyes.util.metrics.Duration._
import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}

trait ScheduledExecutor{
  def scheduledExecutor: ScheduledExecutorService

  def once[B](f: Function0[Future[B]], duration: Duration): Future[B] = once(toScheduledFunction(f), (), duration)

  def once[A, B](f: A => Future[B], message: => A, duration: Duration): Future[B] = {
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

  def forever[A, B](f: A => Future[B], message: => A, duration: Duration): Future[Unit] =
    unfold[A, B, Unit](f, message, duration)(())((z: Unit, b: B) => ((), Some(message)))

  def repeat[B, Z](f: Function0[Future[B]], duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeat(toScheduledFunction(f), (), duration, times)(seed)(fold)

  def repeat[A, B, Z](f: A => Future[B], message: => A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = {
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

  def repeatWhile[A, B, Z](f: A => Future[B], message: => A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z): Future[Z] = {
    if (pred(seed)){
      unfold(f, message, duration)(seed){(z: Z, b: B) =>
        val newZ = fold(z, b)
        (newZ, if (pred(newZ)) Some(message) else None )
      }
    }
    else Future.sync[Z](seed)
  }

  def unfoldWithDelay[A, B, Z](f: A => Future[B], firstMessage: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = {
    @volatile var canceled = false
    once(f, firstMessage, duration).flatMap { b =>
      generator(seed, b) match {
        case (state, Some(a)) if !canceled => unfold(f, a, duration)(state)(generator)
        case (state, _)                    => Future.sync(state)
      }
    } ifCanceled { _ =>
      canceled = true
    }
  }

  def unfoldWithMinimumDuration[A, B, Z](f: A => Future[B], firstMessage: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = {
    @volatile var canceled = false

    def schedule(message: A, state: Z, delay: Duration): Future[Z] = {
      val start = System.currentTimeMillis
      once(f, message, delay) flatMap { b =>
        generator(state, b) match {
          case (state, Some(a)) if !canceled => 
            val nextDelay = duration.ms.length - (System.currentTimeMillis - start)
            schedule(a, state, (nextDelay max 0).milliseconds)

          case (state, _) => Future.sync(state)
        }
      }
    }
      
    schedule(firstMessage, seed, 0.milliseconds).ifCanceled(_ => canceled = true)
  }

  def unfold[A, B, Z](f: A => Future[B], firstMessage: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = {
    val start = System.currentTimeMillis
    @volatile var canceled = false

    def schedule(n: Long, a: => A, state: Z): Future[Z] = {
      val execTime = start + (n * duration.ms.length)
      val delay = (execTime - System.currentTimeMillis).max(0)

      once(f, a, duration).flatMap { b =>
        generator(state, b) match {
          case (state, Some(a)) if !canceled => schedule(n + 1, a, state)
          case (state, _)                    => Future.sync(state)
        }
      } 
    }

    schedule(0, firstMessage, seed).ifCanceled(_ => canceled = true)
  }

  def !@[A, B](f: A => Future[B], message: => A, duration: Duration) = once(f, message, duration)

  def !@~[A, B](f: A => Future[B], message: => A, duration: Duration) = forever(f, message, duration)

  def !@ [A, B, Z](f: A => Future[B], msg: A, duration: Duration, pred: (Z) => Boolean)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeatWhile(f, msg , duration, pred)(seed)(fold)

  def !@ [A, B, Z](f: A => Future[B], msg: A, duration: Duration, times: Int)(seed: Z)(fold: (Z, B) => Z): Future[Z] = repeat(f, msg , duration, times)(seed)(fold)

  def !@ [A, B, Z](f: A => Future[B], msg: => A, duration: Duration)(seed: Z)(generator: (Z, B) => (Z, Option[A])): Future[Z] = unfold(f, msg , duration)(seed)(generator)

  private def toScheduledFunction[A, B](f: Function0[Future[B]]): A => Future[B] = (_: A) => f()
}

trait ScheduledExecutorSingleThreaded extends ScheduledExecutor{
  lazy val scheduledExecutor = Executors.newSingleThreadScheduledExecutor()
}

object ScheduledExecutorSingleThreaded extends ScheduledExecutorSingleThreaded

trait ScheduledExecutorMultiThreaded extends ScheduledExecutor{
  lazy val scheduledExecutor = Executors.newScheduledThreadPool(2)
}

object ScheduledExecutor extends ScheduledExecutorMultiThreaded
