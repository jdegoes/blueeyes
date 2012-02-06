package blueeyes.bkka

import org.specs2.mutable.Specification 
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Scheduler
import akka.actor.PoisonPill
import akka.actor.ActorKilledException
import akka.dispatch.Await
import akka.dispatch.Promise
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.TimeUnit

class StoppableSpec extends Specification with AkkaDefaults {
  val actorSystem = ActorSystem("stoppable_spec")
  implicit val timeout = Timeout(1000)
  val random = new scala.util.Random

  case class TestStopTarget(position: String) {
    val stopActor = actorSystem.actorOf(Props(new Actor { def receive = { case () => Thread.sleep(random.nextInt(100)); sender ! position } }))

    lazy val stop = {
      (stopActor ? ()).flatMap(_ => ActorRefStop(actorSystem, timeout).stop(stopActor)).map(_ => position)
    }
  }

  implicit object TestStop extends Stop[TestStopTarget] {
    def stop(target: TestStopTarget) = target.stop
  }

  "A stoppable instance" should {
    "respect the order of the stopping graph" in {
      val root = new TestStopTarget("r")
      val a1 = new TestStopTarget("a1")
      val a2 = new TestStopTarget("a2")
      val b1a = new TestStopTarget("b1a")
      val b1b = new TestStopTarget("b1b")
      val b2a = new TestStopTarget("b2a")

      val stoppable = Stoppable(
        root, 
        Stoppable(a1, Stoppable(b1a) :: Stoppable(b1b) :: Nil) :: 
        Stoppable(a2, Stoppable(b2a) :: Nil) :: Nil
      )

      Await.result(Stoppable.stop(stoppable), timeout.duration) must_== List("r", "a1", "a2", "b1a", "b1b", "b2a")
    }
  }
}


// vim: set ts=4 sw=4 et:
