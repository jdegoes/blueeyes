package blueeyes.util

import org.spex.Specification


class ActorSpec extends Specification with StrategySequential{
  "Actor: process message" in {
    val actor = Actor[String, String] {case message: String => message + "_done"}

   actor("foo").value must eventually (beSome("foo_done"))
  }
}