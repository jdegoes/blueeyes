package blueeyes.core.http

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import java.util.Date;
import java.lang.UnsupportedOperationException
import java.lang.IllegalArgumentException

sealed trait HttpDateTime extends Rfc1123Standard {

  def storedDate: DateTime  
   
  override def toString: String = rfc1123DateFormat.print(storedDate);
}

sealed trait Rfc1123Standard {
  
  def rfc1123DateFormat: DateTimeFormatter = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZone(DateTimeZone.UTC)

}

object HttpDateTimes extends Rfc1123Standard{

  /* It might be good to add more timezones than just UTC/GMT */
  def parseHttpDateTimes (inString: String): Option[HttpDateTime] = {
    def modString = inString.trim.replaceFirst("UTC", "GMT").replace("-", " ")
    try {
      Some(StandardDateTime(rfc1123DateFormat.parseDateTime(modString)))
    } catch {
      case ex: UnsupportedOperationException => None   // -- parser doesn't work?
      case ex: IllegalArgumentException => None  // Can't parse the text 
    }
  }

  case class StandardDateTime (storedDate: DateTime) extends HttpDateTime 
}

trait HttpDateImplicits {
  implicit def jodaDateTime2HttpDateTime(jodaDateTime: DateTime): HttpDateTime =  {
    HttpDateTimes.StandardDateTime(jodaDateTime)
  } 

  implicit def javaDate2HttpDateTime(javaDate: java.util.Date): HttpDateTime = {
    HttpDateTimes.StandardDateTime(new DateTime(javaDate))
  }
}

object HttpDateImplicits extends HttpDateImplicits 


