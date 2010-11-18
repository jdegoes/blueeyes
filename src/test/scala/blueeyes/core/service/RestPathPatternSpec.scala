package blueeyes.core.service

import org.specs.Specification

import blueeyes.core.http.{HttpRequest, HttpMethods}

class RestPathPatternSpec extends Specification{
  import RestPathPatternImplicits._
  
  "match correct literal path containing a single path element" in {
    testPath("/foo",
      List(("/foo", Map())),
      List("/bar")
    )
  }
  "match correct literal path containing many path elements" in {
    testPath("/foo/bar/baz",
      List(("/foo/bar/baz", Map())),
      List("/bar/baz/bar")
    )
  }
  "match correct literal and symbol path containing many path elements" in {
    testPath("/foo/'bar/baz",
      List(("/foo/baz/baz", Map('bar -> "baz")), ("/foo/boo/baz", Map('bar -> "boo")), ("/foo/foo/baz", Map('bar -> "foo"))),
      List("/bar/bar/bar")
    )
  }
  "match root path" in {
    testPath("/",
      List(("/", Map())),
      List("")
    )
  }
  "create parameters that have characters which are not valid for symbols themselves" in {
    testPath("/'foo",
      List(("/foo$bar-baz", Map('foo -> "foo$bar-baz"))),
      List("")
    )
  }
  "not match more than specified when end method is invoked" in {
    RestPathPattern("/get/'foo").$.isDefinedAt("/foo/bar") mustEqual(false)
  }
  "create parameters for regression case" in {
    testPath("/get/'foo",
      List(("/get/foo-value", Map('foo -> "foo-value"))),
      List("/foo/bar")
    )
  }
  "combine symbols and literals using slash operator" in {
    val pattern: RestPathPattern = "/foo" / 'bar / 'biz / "blah"
    
    pattern("/foo/a/b/blah") mustEqual(Map('bar -> "a", 'biz -> "b"))
  }
  "match manually formed path including root literal" in {
    (RestPathPattern.Root / "foo").isDefinedAt("/foo") mustEqual(true)
  }
  "match complex path with symbols" in {
    (RestPathPattern.Root / "foo" / "bar" / 'param).isDefinedAt("/foo/bar/value") mustEqual(true)
    }

  "match end of path when final element is symbol" in {
    ("/foo/bar/'param" $).apply("/foo/bar/value") mustEqual(Map('param -> "value"))
  }
  "not match beyond end of path when final element is symbol" in {
    ("/foo/bar/'param" $).isDefinedAt("/foo/bar/value/") mustBe(false)
  }
  "match end of path when final element is string" in {
    ("/foo/bar/adCode.html" $).apply("/foo/bar/adCode.html") mustEqual(Map())
  }
  "not match beyond end of path when final element is string" in {
    ("/foo/bar/adCode.html" $).isDefinedAt("/foo/bar/adCode.html2") mustBe(false)
  }

  /* ---- Regex Tests ---- */ 
  "match for a simple regex" in {
    (RestPathPattern.Root/ "foo" / "bar" / """(steamboats)""".r ~ List('id)).isDefinedAt("/foo/bar/steamboats") mustEqual(true)
  }
  "not match for a simple regex"  in {
    (RestPathPattern.Root/ "foo" / "bar" / """(steamboats)""".r ~ List('id)).isDefinedAt("/foo/bar/lame_boats") mustEqual(false)
  }
  "match a more complex regex" in {
    (RestPathPattern.Root/ "foo" / "bar" / """([a-z]+_[0-9])""".r ~ List('id)).isDefinedAt("/foo/bar/hercules_1") mustEqual(true)
  }
  "not match for a more complex regex" in {
    (RestPathPattern.Root/ "foo" / "bar" / """([a-z]+_[0-9])""".r ~ List('id)).isDefinedAt("/foo/bar/HadesSux") mustEqual(false)
  }


  "create single parameter" in {
    (RestPathPattern.Root / 'param).apply("/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "create multiple parameters" in {
    (RestPathPattern.Root / 'param1 / 'param2).apply("/value1/value2") mustEqual(Map[Symbol, String]('param1 -> "value1", 'param2 -> "value2"))
  }
  "create single parameter in lengthy literal path" in {
    (RestPathPattern.Root / "foo" / "bar" / 'param).apply("/foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }

  /* ---- Suffix Tests ---- */
  "match on a symbol with a suffix (ex: 'name.gif)" in {
    ("/foo/bar/'name.gif" $).isDefinedAt("/foo/bar/foocubus.gif") mustBe (true)
  }
  "recover the parameter when using a path with a suffix" in {
    val pattern: RestPathPattern = "/foo/bar/'name.gif"
    pattern.apply("/foo/bar/foocubus.gif").mustEqual(Map[Symbol, String]('name -> "foocubus"))
  }
  "not match on a symbol with different suffix (ex: 'name.html)" in {
    ("/foo/bar/'name.gif" $).isDefinedAt("/foo/bar/example.html") mustBe (false)
  }


  "create parameters automatically for complex path specified as string" in {
    val pattern: RestPathPattern = "/foo/bar/'param"
    
    pattern.apply("/foo/bar/value") mustEqual(Map[Symbol, String]('param -> "value"))
  }
  "shift request uri leftward by matched pattern" in {
    val pattern: RestPathPattern = "/foo/'param"
    
    pattern.shift(HttpRequest(method = HttpMethods.GET, uri = "/foo/bar/baz")) mustEqual(HttpRequest(method = HttpMethods.GET, uri = "/baz"))
  }
  
  private def testPath(path: String, isDefinedAt: List[(String, Map[Symbol, String])], isNotDefinedAt: List[String]) {
    val pattern = RestPathPattern(path)
    
    isDefinedAt.foreach { pair =>
      val path = pair._1
      val map  = pair._2
      
      pattern.isDefinedAt(path) mustEqual(true)
      pattern.apply(path) mustEqual(map)
    }
    
    isNotDefinedAt.foreach { path =>
      pattern.isDefinedAt(path) mustEqual(false)
    }
  }
}
