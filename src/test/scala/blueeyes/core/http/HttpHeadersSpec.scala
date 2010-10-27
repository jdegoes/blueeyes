package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpHeadersSpec extends Specification {
  val encodingExample = "compress"

  /* MimeType stuff */

  "MimeType: \t parsing should parse an example correctly" in {
    val mimeTypeExample = "text/plain; q=0.5, text/html, application/x-dvi; q=0.8, text/x-c, audio/basic"
    val mimeTypeExampleNoQ = "text/plain, text/html, application/x-dvi, text/x-c, audio/basic"
    MimeTypes.parseMimeTypes(mimeTypeExample).map(_.value).mkString(", ") mustEqual mimeTypeExampleNoQ
  }

  "MimeType: \t Should output an empty array with input \"game/minecraft\"" in {
    MimeTypes.parseMimeTypes("game/minecraft").length mustEqual 0
  }

  "MimeType: \t Should produce text/javascript with input \"text/javascript\"" in {
    MimeTypes.parseMimeTypes("text/javascript")(0).value mustEqual "text/javascript"
  } 

  "MimeType: \t Should produce anymaintype/anysubtype with input \"*/*\"" in {
    MimeTypes.parseMimeTypes("*/*")(0).value mustEqual "*/*"
  } 

  "MimeType: \t Should remove q values, producing text/html with \"text/html; q=.6*\"" in {
    MimeTypes.parseMimeTypes("text/html; q=.6")(0).value mustEqual "text/html"
  } 

  "Accept-Type: \t Should be able to create a new instance with \"text/html, video/quicktime, application/json\" " in {
    HttpHeaders.Accept(text/html, video/quicktime, application/json).value mustEqual "text/html, video/quicktime, application/json"
  }

  "CharSets: \t Should produce a charset of type US-ASCII from \"US-ASCII\"" in {
    CharSets.parseCharSets("US-ASCII")(0).value mustEqual "US-ASCII"
  }

  "Accept-CharSet: \t Should produce a charset of type US-ASCII from \"US-ASCII\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("US-ASCII"): _*).value mustEqual "US-ASCII"
  }

  "Accept-CharSet: \t Should produce charsets of type ISO-8859-1, ISO-8859-5 from \"iso-8859-1, ISO_8859_5\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("iso-8859-1, ISO_8859_5"): _*).value mustEqual "ISO-8859-1, ISO-8859-5"
  }

  "Accept-CharSet: \t Should produce custom charset of type spaceranger from \"SpaceRanger\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("SpaceRanger"): _*).value mustEqual "spaceranger"
  }

  "Content-Type: \t Should be able to create a new instance with text/html" in {
    HttpHeaders.`Content-Type`(text/html).value mustEqual "text/html"
  }

  "Content-Type: \t Should be */* with anymaintype/anysubtype" in {
    HttpHeaders.`Content-Type`(anymaintype/anysubtype).value mustEqual "*/*"
  } 

  "Language-Range: \t Should produce en-uk from \"en-uk\"" in {
    LanguageRanges.parseLanguageRanges("en-uk")(0).value mustEqual "en-uk"
  }

  "Accept-Language: \t Should create languages (en-us-calif, is, cn) from \"en-us-calif, is=now!, cn; q=1\""  in {
    HttpHeaders.`Accept-Language`(LanguageRanges.parseLanguageRanges("en-us-calif, is=now!, cn; q=1"): _*)
      .value mustEqual "en-us-calif, is, cn"
  }

  "Range-Units: \t Should parse \"bytes\" as Some(bytes) produce None for \"cats\"" in {
    RangeUnits.parseRangeUnits("bytes").map(_.toString).getOrElse("") mustEqual "bytes"
    RangeUnits.parseRangeUnits("cats").map(_.toString).getOrElse("") mustEqual ""
  }

  "Accept-Ranges: \t Should create none from \"none\"" in {
    HttpHeaders.`Accept-Ranges`(RangeUnits.parseRangeUnits("none").get).value mustEqual "none"
  }

  "Authorization: \t Should create awea9f83hf23f90 from \"awea9f83hf23f90\"" in {
    HttpHeaders.`Authorization`("awea9f83hf23f90").value mustEqual "awea9f83hf23f90"
  }
}
