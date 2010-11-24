package blueeyes.core.service.engines

import org.spex.Specification
import blueeyes.util.Future

class FuturesListSpec extends Specification{
  private val future = new Future[Int]()
  private val list = new FuturesList[Int]()

  "adds future" in{
    list + future

    list.futures() mustEqual(future :: Nil)
  }
  "removes future" in{
    list + future
    list - future

    list.futures() mustEqual(Nil)
  }
  "removes delivered future" in{
    list + future
    future.deliver(1)
    list - future

    list.futures() mustEqual(Nil)
  }
  "cancel futures" in{
    list + future
    list.cancel

    future.isCanceled must be (true)
  }
  "cancel future if list already cancelled" in{
    list.cancel    
    list + future

    future.isCanceled must be (true)
  }
}