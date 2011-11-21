package blueeyes.concurrent

import Future._
import org.specs2.mutable.Specification
import org.specs2.time.TimeConversions._

import org.specs2.specification.BeforeAfterExample
import akka.actor.{Actor, ActorRef}

class FutureImplicitsSpec extends Specification with BeforeAfterExample{

  override def is = args(sequential = true) ^ super.is

  private var actor: ActorRef = _
  "FutureImplicitsSpec" should{
    "convert Akka future when value is delivered" in{
      val result = actor.?("success").mapTo[String]
      val future = result.toBlueEyes
      future.value must eventually (beSome("foo"))
    }

    "convert Akka future for long running actor when value is delivered" in{
      implicit val timeout = Actor.Timeout(10000)
      val result = actor.?("long").mapTo[String]
      val future = result.toBlueEyes
      future.value must eventually(20, 1.second) (beSome("foo"))
    }

    "convert Akka future when future has error" in{
      val result = actor.?("error").mapTo[String]
      val future = result.toBlueEyes
      future.isCanceled must eventually (be_==(true))
    }
  }

  protected def before = {
    actor = Actor.actorOf[GetActor]
    actor.start
  }

  protected def after = actor.stop
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
