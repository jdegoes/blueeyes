package blueeyes.util

trait Future[+T] { self =>
  /*def map[S](g: T => S): Future[T] = {
    val f = new Future[S] {
      def foreach(f: S => Unit): Unit = self.foreach(f.compose(g))
    }
  }*/
  
  def flatten[S](implicit witness: T => Future[S]): Future[S]
  
  def flatMap[S](f: T => Future[S]): Future[S] 
  
  def filter(f: T => Boolean): Future[T]
  
  def foreach(f: T => Unit): Unit
  
  def value(): Option[T]
  
  def deliverTo(f: T => Unit): Future[T] = { foreach(f); this }
  
  def whenDone(f: => Unit): Future[T]
}

object Future {

  /*def Done[T] = new Future[T] {
    def foreach(f: T => Unit): Unit = { }
    
    
  }*/
}