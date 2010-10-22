package blueeyes.core.http

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

  def rfc1123DateFormat: DateTimeFormatter = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZone(DateTimeZone.UTC)

  case class HttpDateData (inDate: DateTime) extends HttpDateTime {
    override def storedDate = inDate
  }

  object HttpDateData {
    def apply(stringDate: String): HttpDateTime = HttpDateData(rfc1123DateFormat.parseDateTime(stringDate))
  }
    
}

trait HttpDateImplicits {

  //case class HttpDateConversion(storedDate: DateTime) extends HttpDateTime

  implicit def jodaDateTime2HttpDateTime(jodaDateTime: DateTime): HttpDateTime =  {
    case class HttpDateConversionJoda(jodaTime: DateTime) extends HttpDateTime {
      override def storedDate = jodaTime
    }
    return HttpDateConversionJoda(jodaDateTime)
  } 

  implicit def javaDate2HttpDateTime(javaDate: java.util.Date): HttpDateTime = {
    case class HttpDateConversionJava (javaTime: DateTime ) extends HttpDateTime {
      override def storedDate = javaTime 
    }
    return HttpDateConversionJava(new DateTime(javaDate))
  }
}

object HttpDateImplicits extends HttpDateImplicits 


