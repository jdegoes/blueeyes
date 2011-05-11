package blueeyes.concurrent

import java.util.concurrent.{SynchronousQueue, ThreadPoolExecutor, TimeUnit}

object ActorStrategy extends ActorStrategyMultiThreaded {
  lazy val executorService = new ThreadPoolExecutor(2, 1000, 10*60, TimeUnit.SECONDS, new SynchronousQueue())
}