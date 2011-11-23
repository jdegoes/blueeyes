package blueeyes.bkka

import org.specs2.mutable.Specification 
import akka.actor.Actor
import akka.actor.Scheduler
import akka.actor.PoisonPill
import akka.actor.ActorKilledException
import java.util.concurrent.TimeUnit

class StoppableSpec extends Specification {
  implicit val timeout = Actor.Timeout(1000)
  val random = new scala.util.Random

  case class TestStopTarget(position: String) {
    val stopActor = Actor.actorOf(new Actor { def receive = { case () => Thread.sleep(random.nextInt(100)); self.reply(position) } }).start()

    lazy val stop = (stopActor ? ()).flatMap(_ => (stopActor ? PoisonPill) recover { case ex: ActorKilledException => () }).map(_ => position)
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

      Stoppable.stop(stoppable).get must_== List("r", "a1", "a2", "b1a", "b1b", "b2a")
    }
  }
}


// vim: set ts=4 sw=4 et:
