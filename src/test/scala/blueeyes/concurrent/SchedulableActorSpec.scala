package blueeyes.concurrent

import org.specs.Specification
import ScheduledActor._
import Duration._

class SchedulableActorSpec extends Specification with FutureDeliveryStrategySequential{

  private val actorImplementation = new ActorImplementationSequential{}

  "SchedulableActor.once" should {
    "execute function" in{
      import actorImplementation._
      val messageProcessor = actor[String, String] {
        case message: String => message + "_done"
      }

      val f = messageProcessor !@ ("foo", 10.milliseconds)

      f.value must eventually (beSome("foo_done"))
    }
  }
}