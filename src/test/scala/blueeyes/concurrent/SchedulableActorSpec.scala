package blueeyes.concurrent

import org.specs.Specification
import ScheduledActor._
import Duration._

class SchedulableActorSpec extends Specification with FutureDeliveryStrategySequential{

  "SchedulableActor.once" should {
    "execute function" in{
      val messageProcessor = new Actor with ActorStrategySequential {
        val func = lift1{message: String => message + "_done"}
      }

      val f = messageProcessor.func !@ ("foo", 10.milliseconds)

      f.value must eventually (beSome("foo_done"))
    }
  }
}