package blueeyes.core.http

import org.specs.Specification

class HttpDateTimeSpec extends Specification {

  "Date:  Should return an HttpDate object with correct inputs" in {
    HttpHeaders.Date(HttpDateTimes.parseHttpDateTimes("  MON, 01-JAN-2001 00:00:00 UTC  ").get).value mustEqual "Mon, 01 Jan 2001 00:00:00 GMT"
  }
  
  "Date: Should return an HttpDate object given correct inputs with suspect capitalization" in {
    HttpHeaders.Date(HttpDateTimes.parseHttpDateTimes("tue, 29 dec 2009 12:12:12 GMT  ").get).value mustEqual "Tue, 29 Dec 2009 12:12:12 GMT"
  }

  "Date:  Should return none for badly formatted date" in {
    HttpDateTimes.parseHttpDateTimes("Mon, 01-Jan-2001 00:00:00 UTC fooo baaaaar") mustEqual None
  }

}

