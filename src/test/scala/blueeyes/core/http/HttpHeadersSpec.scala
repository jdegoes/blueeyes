package blueeyes.core.http

import org.specs.Specification
import blueeyes.core.http.MimeTypes._

class HttpHeadersSpec extends Specification {
  val encodingExample = "compress"

  /* MimeType stuff */

  /* Three cases for most: Good info, Good + malformed info, Malformed info */

  "MimeType:  parsing should parse an example correctly" in {
    val mimeTypeExample = "text/plain; q=0.5, text/html, application/x-dvi; q=0.8, text/x-c, audio/basic"
    val mimeTypeExampleNoQ = "text/plain, text/html, application/x-dvi, text/x-c, audio/basic"
    MimeTypes.parseMimeTypes(mimeTypeExample).map(_.value).mkString(", ") mustEqual mimeTypeExampleNoQ
  }

  "MimeType:  Should output an empty array with input \"game/minecraft\"" in {
    MimeTypes.parseMimeTypes("game/minecraft").length mustEqual 0
  }

  "MimeType:  Should output an text/plain from input \"game/minecraft, text/plain\"" in {
    MimeTypes.parseMimeTypes("game/minecraft, text/plain")(0).value mustEqual "text/plain"
  }

  "MimeType:  Should produce text/javascript with input \"text/javascript\"" in {
    MimeTypes.parseMimeTypes("text/javascript")(0).value mustEqual "text/javascript"
  } 

  "MimeType:  Should produce anymaintype/anysubtype with input \"*/*\"" in {
    MimeTypes.parseMimeTypes("*/*")(0).value mustEqual "*/*"
  } 

  "MimeType:  Should remove q values, producing text/html with \"text/html; q=.6\"" in {
    MimeTypes.parseMimeTypes("text/html; q=.6")(0).value mustEqual "text/html"
  } 


  "Accept-Type:  Should be able to create a new instance with \"text/html, video/quicktime, application/json\" " in {
    HttpHeaders.Accept(text/html, video/quicktime, application/json).value mustEqual "text/html, video/quicktime, application/json"
  }
  
  "CharSets:  Should produce a charset of type US-ASCII from \"US-ASCII\"" in {
    CharSets.parseCharSets("US-ASCII")(0).value mustEqual "US-ASCII"
  }

  "Accept-CharSet:  Should produce a charset of type US-ASCII from \"US-ASCII\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("US-ASCII"): _*).value mustEqual "US-ASCII"
  }

  "Accept-CharSet:  Should produce charsets of type ISO-8859-1, ISO-8859-5 from \"iso-8859-1, ISO_8859_5\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("iso-8859-1, ISO_8859_5"): _*).value mustEqual "ISO-8859-1, ISO-8859-5"
  }

  "Accept-CharSet:  Should produce custom charset of type spaceranger from \"SpaceRanger\"" in {
    HttpHeaders.`Accept-Charset`(CharSets.parseCharSets("SpaceRanger"): _*).value mustEqual "spaceranger"
  }

  "Content-Type:  Should be able to create a new instance with text/html" in {
    HttpHeaders.`Content-Type`(text/html).value mustEqual "text/html"
  }

  "Content-Type:  Should be */* with anymaintype/anysubtype" in {
    HttpHeaders.`Content-Type`(anymaintype/anysubtype).value mustEqual "*/*"
  } 

  "Language-Range:  Should produce en-uk from \"en-uk\"" in {
    LanguageRanges.parseLanguageRanges("en-uk")(0).value mustEqual "en-uk"
  }

  "Accept-Language:  Should create languages (en-us-calif, is, cn) from \"en-us-calif, is=now!, cn; q=1\""  in {
    HttpHeaders.`Accept-Language`(LanguageRanges.parseLanguageRanges("en-us-calif, is=now!, cn; q=1"): _*)
      .value mustEqual "en-us-calif, is, cn"
  }

  "Range-Units:  Should parse \"bytes\" as Some(bytes) produce None for \"cats\"" in {
    RangeUnits.parseRangeUnits("bytes").map(_.toString).getOrElse("") mustEqual "bytes"
    RangeUnits.parseRangeUnits("cats").map(_.toString).getOrElse("") mustEqual ""
  }

  "Accept-Ranges:  Should create none from \"none\"" in {
    HttpHeaders.`Accept-Ranges`(RangeUnits.parseRangeUnits("none").get).value mustEqual "none"
  }

  "Authorization:  Should create awea9f83hf23f90 from \"awea9f83hf23f90\"" in {
    HttpHeaders.`Authorization`("awea9f83hf23f90").value mustEqual "awea9f83hf23f90"
  }

  "Connection:  Should return \"foo\" when passeed \" foo 0r91j2 \\n\"." in {
    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens(" foo 0r91j2\n ").get).value mustEqual "foo"
  }
  "Connection:  Should create a new connection header from \"close\"." in {
    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens("close").get).value mustEqual "close"
  }

  "Connection:  Should create a custom connection header from the depreciated \" Keep-Alive \"." in {
    HttpHeaders.Connection(ConnectionTokens.parseConnectionTokens(" Keep-Alive ").get).value mustEqual "Keep-Alive"
  }

  "HttpCookie:  Should create a new cookie " in {
    HttpHeaders.Cookie(HttpCookies.parseHttpCookies("Cat=Mittens; expires=Mon, 01-Jan-2001 00:00:00 UTC; path=/; domain=.kittens.com").get).value mustEqual "Cat=Mittens; expires=Mon, 01 Jan 2001 00:00:00 GMT; path=/; domain=.kittens.com"
  }

  "Content-Length: Should return ContentLength or parse to None on bad input" in {
    HttpHeaders.`Content-Length`(HttpNumbers.parseHttpNumbers("5").get).value mustEqual "5"
    HttpNumbers.parseHttpNumbers("bees") mustEqual None
  }

  "Content-Type: Should return a Content Type with MimeType inputs" in {
    HttpHeaders.`Content-Type`(MimeTypes.parseMimeTypes("multipart/mixed, application/*"): _*).value mustEqual "multipart/mixed, application/*"
  }

  "Date: Should return an HttpDate object with correct inputs" in {
    HttpHeaders.Date(HttpDateTimes.parseHttpDateTimes("  MON, 01-JAN-2001 00:00:00 UTC  ").get).value mustEqual "Mon, 01 Jan 2001 00:00:00 GMT"
    HttpHeaders.Date(HttpDateTimes.parseHttpDateTimes("tue, 29 dec 2009 12:12:12 GMT  ").get).value mustEqual "Tue, 29 Dec 2009 12:12:12 GMT"
  }

  "Date: Should return none for badly formatted date" in {
    HttpDateTimes.parseHttpDateTimes("Mon, 01-Jan-2001 00:00:00 UTC fooo baaaaar") mustEqual None
  }

  "Expectation: Should return (Some of) continue or failure on good Inputs and parse to None on bad input" in {
    HttpHeaders.Expect(Expectations.parseExpectations("100").get).value mustEqual "100-continue"
    HttpHeaders.Expect(Expectations.parseExpectations("417").get).value mustEqual "417-expectationfailed"
    Expectations.parseExpectations("asdf4s17") mustEqual None
  }

  "From: Should return the correct email name with a well-formed email and parse to None otherwise" in {
    HttpHeaders.From(HttpUris.parseEmails("johnsmith@socialmedia.com").get).value mustEqual "johnsmith@socialmedia.com"
    HttpHeaders.From(HttpUris.parseEmails(" j.o.n.Sm.ith@so.cia.lmedia.com ").get).value mustEqual "j.o.n.Sm.ith@so.cia.lmedia.com"
    HttpUris.parseEmails("209h3094)(it092jom") mustEqual None
  }

  "Host: Should return correct host uri and parse to None otherwise" in {
    HttpHeaders.Host(HttpUris.parseHttpUris("http://www.socialmedia.com/coolServer/index.html").get).value mustEqual "www.socialmedia.com"
    HttpHeaders.Host(HttpUris.parseHttpUris("http://maps.google.com/coolmap.html").get).value mustEqual "maps.google.com"
    HttpUris.parseHttpUris("@^#&(!_") mustEqual None
  }

  "If-Match: Should return strings on well-formed input" in {
    HttpHeaders.`If-Match`(EntityTags.parseEntityTags("\"c4tattack\", \"cyberTiger\"").get).value mustEqual "\"c4tattack\", \"cybertiger\"" 
    }

  "If-Match: Should return * string on presence of *; also, parser returns None on malformed input" in {
    HttpHeaders.`If-Match`(EntityTags.parseEntityTags("*, \"c4tattack\", \"cyberTiger\"").get).value mustEqual "*"
    EntityTags.parseEntityTags("w%015") mustEqual None
  }

  "Location: Should return correct url on parsed input" in {
    HttpHeaders.`Location`(HttpUris.parseHttpUris("  http://www.socialmedia.com/index.html  ").get).value mustEqual "http://www.socialmedia.com/index.html"    
  }

  "Location: Parsing should return none on bad input" in {
    HttpUris.parseHttpUris("&%*#(!)Thttp://.socialmedia.com") mustEqual None
  }

  "Pragma: Parsing should return 'no-cache' or None "  in {
    HttpHeaders.Pragma(PragmaDirectives.parsePragmaDirectives(" No-Cache ").get).value mustEqual ("no-cache")
    PragmaDirectives.parsePragmaDirectives(" zom ") mustEqual None
  }

  "Range: Should parse correctly on good input" in {
    HttpHeaders.Range(ByteRanges.parseByteRanges("bytes=0-500, 699-2000, -4").get).value mustEqual "bytes=0-500, 699-2000, -4"
  }

  "Range: Should produce none on bad input" in {
    ByteRanges.parseByteRanges("bytes=cats") mustEqual None
  }

}

