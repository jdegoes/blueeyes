package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class ConnectionTokenSpec extends Specification {

  "Connection:  Should return \"foo\" when passeed \" foo 0r91j2 \\n\"." in {
    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens(" foo 0r91j2\n ").get).value mustEqual "foo"
  }
  "Connection:  Should create a new connection header from \"close\"." in {
    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens("close").get).value mustEqual "close"
  }

  "Connection:  Should create a custom connection header from the depreciated \" Keep-Alive \"." in {    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens(" Keep-Alive ").get).value mustEqual "Keep-Alive"
  }
}
