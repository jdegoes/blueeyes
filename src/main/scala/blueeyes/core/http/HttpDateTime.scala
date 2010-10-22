package blueeyes.core.http

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import java.util.Date;


sealed trait HttpDateTime extends Rfc1123Standard {

  def storedDate: DateTime  
   
  override def toString: String = rfc1123DateFormat.print(storedDate);
}

sealed trait Rfc1123Standard {
  
  def rfc1123DateFormat: DateTimeFormatter = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZone(DateTimeZone.UTC)

}

object HttpDateTimes extends Rfc1123Standard{

  /* Need to think about how to deal with poorly-formed dates */
  def parseHttpDateTimes (inString: String): HttpDateTime = {
    var initDate: DateTime = rfc1123DateFormat.parseDateTime(inString)
    if (initDate == null) 
      return NullDate(new DateTime)
    else 
      return StandardDateTime (initDate)
  }

  case class StandardDateTime (storedDate: DateTime) extends HttpDateTime 

  case class NullDate(storedDate: DateTime) extends HttpDateTime {
    override def toString = ""
  }

  object HttpDateTimes {
    def apply(inString: String): HttpDateTime = parseHttpDateTimes(inString) 
  }
    
}

trait HttpDateImplicits extends Rfc1123Standard{

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


