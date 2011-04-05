package blueeyes.core.http

import org.specs.Specification

class CharSetSpec extends Specification {

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
}

