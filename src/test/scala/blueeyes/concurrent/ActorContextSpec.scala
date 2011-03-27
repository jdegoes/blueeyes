package blueeyes.concurrent

import org.spex.Specification


class ActorContextSpec extends Specification{

  import StrategyWorker._
  "Context is initialized by actor" in {
    var contextIsSet = false

    val actor = Actor[String, Unit] {
      case message: String => contextIsSet = ActorContext.get != None
    }

    actor("foo")

    contextIsSet must eventually (be(true))
  }
  "Context is not set in not actor thread" in {
    val actor = Actor[String, Unit] {
      case message: String =>
    }

    actor("foo")

    ActorContext.get must be(None)
  }
}