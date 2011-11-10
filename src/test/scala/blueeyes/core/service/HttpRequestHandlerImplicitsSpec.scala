package blueeyes.core.service

import org.specs2.mutable.Specification

import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import blueeyes.core.http._
import org.specs2.matcher.MustThrownMatchers

class HttpRequestHandlerImplicitsSpec extends Specification with HttpRequestHandlerImplicits  with MustThrownMatchers{
  "HttpRequestHandlerImplicits.identifierToIdentifierWithDefault: creates IdentifierWithDefault" in {
    import HttpRequestHandlerImplicits._
    val identifierWithDefault = 'foo ?: "bar"
    identifierWithDefault.default mustEqual(Some("bar"))
  }
}

