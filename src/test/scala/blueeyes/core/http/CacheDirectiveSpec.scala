package blueeyes.core.http

import org.specs.Specification

class CacheDirectiveSpec extends Specification {

  "Cache-Directive: Should parse a cache directive with a field correctly" in {
    val testString1 = "private=\"this\", no-cache, max-age=10, no-transform"
    HttpHeaders.`Cache-Control`(CacheDirectives.parseCacheDirectives(testString1): _*).value mustEqual testString1
  }  

  "Cache-Directive: Should parse a cache-directive with a delta " in {
    val testString2 = "private, no-cache, max-stale=590, no-transform"
    HttpHeaders.`Cache-Control`(CacheDirectives.parseCacheDirectives(testString2): _*).value mustEqual testString2
  }

  "Cache-Directive: Should return empty array on bad input" in {
    val testString3 = "amnamzimmeram"
    CacheDirectives.parseCacheDirectives(testString3).length mustEqual 0
  }

}

