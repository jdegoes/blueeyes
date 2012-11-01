package blueeyes.util

import java.io.{StringWriter, PrintWriter}

case class RichThrowable(t: Throwable) {
  def fullStackTrace: String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw, true)
    
    t.printStackTrace(pw)
    
    pw.flush()
    sw.flush()
    
    sw.toString()
  }
}

trait RichThrowableImplicits {
  implicit def throwable2RichThrowable(t: Throwable): RichThrowable = new RichThrowable(t)
}
object RichThrowableImplicits extends RichThrowableImplicits