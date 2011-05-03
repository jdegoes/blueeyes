package blueeyes.concurrent

import org.specs.Specification

class ActorContextSpec extends Specification{
  private var contextIsSet = false

  val actor1 = new Actor with ActorStrategyMultiThreaded  {
    val func = lift1{ v: String =>
      contextIsSet = ActorContext.get != None
    }
  }
  val actor2 = new Actor with ActorStrategyMultiThreaded  {
    val func = lift1{ v: String =>
      contextIsSet = ActorContext.get != None
    }
  }

  "Context is initialized by actor" in {

    actor1.func("foo")

    contextIsSet must eventually (be(true))
  }
  "Context is not set in not actor thread" in {
    actor1.func("foo")

    ActorContext.get must be(None)
  }
}