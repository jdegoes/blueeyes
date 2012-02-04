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

  val defaultFutureDispatch: MessageDispatcher = actorSystem.dispatchers.lookup("blueeyes_async")
}

// vim: set ts=4 sw=4 et:
