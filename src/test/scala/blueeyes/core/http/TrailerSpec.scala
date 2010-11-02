package blueeyes.core.http

import org.specs.Specification

class TrailerSpec extends Specification {

  "Trailer/HttpHeaderFields: Should parse correctly, if parsing for trailer" in {
    HttpHeaders.Trailer(HttpHeaderFields.parseHttpHeaderFields("Accept, Age, Date, Max-Forwards, Content-Length", "trailer").get: _*).value mustEqual "Accept, Age, Date, Max-Forwards"
  }

  "Trailer/HttpHeaderFields: Should also prarse correctly if not parsing fo the trailer" in {
    HttpHeaderFields.parseHttpHeaderFields("Accept, Age, Date, Max-Forwards, Cats, Content-Length", "").get.map(_.toString).mkString(", ") mustEqual "Accept, Age, Date, Max-Forwards, Content-Length"
  }

}

