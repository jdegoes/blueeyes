package blueeyes.bkka

import akka.actor.ActorSystem
import akka.dispatch.MessageDispatcher
import akka.util.duration._

trait AkkaDefaults {
  def defaultActorSystem = AkkaDefaults.actorSystem
  implicit def defaultFutureDispatch = AkkaDefaults.defaultFutureDispatch
}

object AkkaDefaults {
  val actorSystem = ActorSystem.create("blueeyes_actors")

  val defaultFutureDispatch: MessageDispatcher = 
    actorSystem.dispatcherFactory.newDispatcher("blueeyes_async")
    .withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity.setCorePoolSize(8)
    .setMaxPoolSize(100).setKeepAliveTime(30 seconds).build
}

// vim: set ts=4 sw=4 et:
