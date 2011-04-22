package blueeyes.concurrent

import collection.mutable.ListBuffer

trait FutureDeliveryStrategy {
 def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit): Unit
}

trait FutureDeliveryStrategySequential {
 implicit val futureDeliveryStrategy = new FutureDeliveryStrategy {
   def deliver[A](value: A, listeners: Iterable[A => Unit], errorHandler: List[Throwable] => Unit) = {
     val buffer = new ListBuffer[Throwable]

     listeners foreach { listener =>
       try listener(value)
       catch {
         case t: Throwable => buffer += t
       }
     }

     if (buffer.length > 0) errorHandler(buffer.toList)
   }
 }
}
