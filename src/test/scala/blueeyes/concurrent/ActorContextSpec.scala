package blueeyes.concurrent

import org.spex.Specification


class ActorContextSpec extends Specification{

  import Actor._
  "Context is initialized by actor" in {
    var contextIsSet = false

    val actor1 = actor[String, Unit] {
      case message: String => contextIsSet = ActorContext.get != None
    }

    actor1("foo")

    contextIsSet must eventually (be(true))
  }
  "Context is not set in not actor thread" in {
    val actor1 = actor[String, Unit] {
      case message: String =>
    }

    actor1("foo")

    ActorContext.get must be(None)
  }
}