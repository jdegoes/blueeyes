package blueeyes.core.http

import org.specs2.mutable.Specification
import org.specs2.matcher.MustThrownMatchers

class TrailerSpec extends Specification with MustThrownMatchers {

  "Trailer/HttpHeaderFields: Should parse correctly, if parsing for trailer" in {
    HttpHeaders.Trailer(HttpHeaderField.parseAll("Accept, Age, Date, Max-Forwards, Content-Length", "trailer"): _*).value mustEqual "Accept, Age, Date, Max-Forwards"
  }

  "Trailer/HttpHeaderFields: Should also prarse correctly if not parsing fo the trailer" in {
    HttpHeaderField.parseAll("Accept, Age, Date, Max-Forwards, Cats, Content-Length", "").map(_.toString).mkString(", ") mustEqual "Accept, Age, Date, Max-Forwards, Content-Length"
  }

}

