package blueeyes.util

import org.specs.Specification
import org.specs.util._
import blueeyes.util.Future
import blueeyes.util.FutureImplicits._

class FutureSpec extends Specification {
  val duration = 250
  val retries = 10
  
  "Futures should support cancel" in { 
    def returnsAFuture(t: String): Future[String] = { 
      val f = new Future[String]()
      f.cancel(new Exception("canceled future"))
      f
    }

    returnsAFuture("test").error must eventually(retries, new Duration(duration))(beSomething)
  }

  "Futures should support cancel with a canceled flatMap chain" in { 
    def returnsAFuture(t: String): Future[String] = { 
      val f = new Future[String]()
      f.cancel(new Exception("canceled future"))
      f
    }
    
    returnsAFuture("test").flatMap { s =>
      "future should not deliver this handler"
    }.error must eventually(retries, new Duration(duration))(beSomething)
  }
}
