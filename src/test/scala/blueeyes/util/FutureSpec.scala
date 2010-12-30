package blueeyes.util

import org.specs.Specification
import org.specs.util._
import blueeyes.util.FutureImplicits._

class FutureSpec extends Specification {
  "Future" should {
    "support cancel" in { 
      val f = Future.dead[String](new Exception("error"))

      f.error must eventually (beSomething)
    }
  }
  
  "Future.deliver" should {
    "automatically canceled when delivering a lazy value that throws an error" in {
      val e = new Exception("foo")
      val f = new Future[String]()

      f.deliver(throw e)

      f.error must eventually (beEqualTo(Some(e)))
    }
    
    "deliver to a second listener even when the first one throws an error" in {
      val f = new Future[String]()
      var result: Option[String] = None
      
      f.deliverTo { s => 
        error("misbehaving delivery handler")
      }.deliverTo { s => 
        result = Some(s)
      }
      
      f.deliver("foo")
      
      result must eventually (beEqualTo(Some("foo")))
    }
  }
  
  "Future.map" should {
    "propagate cancel" in {
      val f = Future.dead[String](new Exception("error"))
      
      f.map(s => s + s).error must eventually (beSomething)
    }
    
    "cancel mapped future when mapping function throws error" in {
      val e = new Exception("foo")
      
      val f = Future("foo")
      
      f.map { string =>
        throw e
      }.error must eventually (beEqualTo(Some(e)))
    }
  }
  
  "Future.flatMap" should {
    "propagate cancel" in { 
      val f = Future.dead[String](new Exception("error"))
    
      f.flatMap { s =>
        "future should not deliver this handler"
      }.error must eventually (beSomething)
    }
    
    "cancel mapped future when mapping function throws error" in {
      val e = new Exception("foo")
      
      val f = Future("foo")
      
      f.flatMap { string =>
        throw e
      }.error must eventually (beEqualTo(Some(e)))
    }
  }
  
  "Future.filter" should {
    "cancel filtered future when filtering function throws error" in {
      val e = new Exception("foo")
      
      val f = Future("foo")
      
      f.filter { string =>
        throw e
      }.error must eventually (beEqualTo(Some(e)))
    }
  }
}
