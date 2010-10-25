package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpHeadersSpec extends Specification {
  val encodingExample = "compress"

  "MimeType parsing should parse an example correctly" in {
    val mimeTypeExample = "text/plain; q=0.5, text/html, application/x-dvi; q=0.8, text/x-c, audio/basic"
    val mimeTypeExampleNoQ = "text/plain, text/html, application/x-dvi, text/x-c, audio/basic"
    MimeTypes.parseMimeTypes(mimeTypeExample).map(_.value).mkString(", ") mustEqual mimeTypeExampleNoQ
  }

  "Content-Type: Should be able to create a new instance with text/html" in {
    HttpHeaders.`Content-Type`(text/html).value mustEqual "text/html"
  }

}
