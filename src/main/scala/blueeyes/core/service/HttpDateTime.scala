package blueeyes.core.service

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import java.util.Date;


sealed trait HttpDateTime {

  def storedDate: DateTime
   
  def rfc1123DateFormat: DateTimeFormatter = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZone(DateTimeZone.UTC)

  override def toString: String = rfc1123DateFormat.print(storedDate);
}


object HttpDateData {

  case class HttpDateData (storedDate: DateTime) extends HttpDateTime {

    def apply(stringDate: String): HttpDateTime = new HttpDateData(rfc1123DateFormat.parseDateTime(stringDate))

  }

}

trait HttpDateImplicits extends HttpDateTime {

  case class HttpDateConversion(storedDate: DateTime) extends HttpDateTime

  implicit def jodaDateTime2HttpDateTime(jodaDateTime: DateTime): HttpDateTime =  {
    HttpDateConversion(jodaDateTime)
  } 

  implicit def javaDate2HttpDateTime(javaDate: java.util.Date): HttpDateTime = {
    HttpDateConversion(new DateTime(javaDate)) 
  }

}

