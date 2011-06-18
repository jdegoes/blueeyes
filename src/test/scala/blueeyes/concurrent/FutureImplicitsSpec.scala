package blueeyes.concurrent

import Future._
import org.specs.Specification
import org.specs.util.TimeConversions._

import akka.actor.Actor

class FutureImplicitsSpec extends Specification {
  "FutureImplicitsSpec" should{
    "convert Akka future when value is delivered" in{
      val actor = Actor.actorOf[GetActor]
      actor.start

      val result = actor !!! "success"
      result.toBlueEyes.value must eventually (beSome("foo"))

      actor.stop
    }

    "convert Akka future for long running actor when value is delivered" in{
      val actor = Actor.actorOf[GetActor]
      actor.start

      val result = actor !!! ("long", 10000)
      result.toBlueEyes.value must eventually(20, 1.second) (beSome("foo"))

      actor.stop
    }

    "convert Akka future when future has error" in{
      val actor = Actor.actorOf[GetActor]
      actor.start

      val result = actor !!! "error"
      result.toBlueEyes.isCanceled must eventually (be(true))

      actor.stop
    }
  }


}

class GetActor extends Actor{
  def receive = {
    case "success" =>
      self.reply("foo")
    case "long" =>
      Thread.sleep(5000)
      self.reply("foo")
    case "error" =>
      throw new RuntimeException("error")
    case _ => sys.error("Unknown")
  }
}
