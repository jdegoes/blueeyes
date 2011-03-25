package blueeyes.concurrent

import scalaz.{Success, Failure, Validation}
import collection.mutable.ListBuffer

trait FutureDeliveryStrategy {
 def deliver[A](value: A, listeners: Iterable[A => Unit]): Validation[List[Throwable], Unit]
}

trait FutureDeliveryStrategySequential {
 implicit val futureDeliveryStrategy = new FutureDeliveryStrategy {
   def deliver[A](value: A, listeners: Iterable[A => Unit]): Validation[List[Throwable], Unit] = {
     val buffer = new ListBuffer[Throwable]

     listeners foreach { listener =>
       try listener(value)
       catch {
         case t: Throwable => buffer += t
       }
     }

     if (buffer.length > 0) Failure(buffer.toList)
     else Success(())
   }
 }
}
