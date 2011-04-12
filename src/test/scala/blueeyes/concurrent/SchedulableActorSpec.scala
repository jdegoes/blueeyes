package blueeyes.concurrent

import java.util.concurrent.TimeUnit
import org.specs.Specification
import org.specs.util.Duration
import ScheduledActor._

class SchedulableActorSpec extends Specification with FutureDeliveryStrategySequential{

  private val actorImplementation = new ActorImplementationSequential{}

  "SchedulableActor.once" should {
    "execute function" in{
      import actorImplementation._
      val messageProcessor = actor[String, String] {
        case message: String => message + "_done"
      }

      val f = messageProcessor !@ ("foo", 10, TimeUnit.MILLISECONDS)

      f.value must eventually (beSome("foo_done"))
    }
  }
}