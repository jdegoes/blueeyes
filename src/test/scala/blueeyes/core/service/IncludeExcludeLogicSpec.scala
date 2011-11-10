package blueeyes.core.service

import org.specs2.mutable.Specification
import util.matching.Regex
import org.specs2.matcher.MustThrownMatchers

class IncludeExcludeLogicSpec extends Specification with MustThrownMatchers{
  "IncludeExcludeLogic" should{
    "include if neither einclude nor exclude are specified" in{
      new IncludeExcludeLogic(Nil, Nil)("/foo") must be_==(true)
    }
    "include if include is specified and path matches" in{
      new IncludeExcludeLogic(List(new Regex("/foo/.+")), Nil)("/foo/bar") must be_==(true)
      new IncludeExcludeLogic(List(new Regex("/foo/bar")), Nil)("/foo/bar") must be_==(true)
    }
    "does not include if include is specified and path does not match" in{
      new IncludeExcludeLogic(List(new Regex("/foo/.*")), Nil)("/bar/foo") must be_==(false)
    }
    "include if exclude is specified and path does not match" in{
      new IncludeExcludeLogic(Nil, List(new Regex("/foo/.*")))("/bar/foo") must be_==(true)
    }
    "does not include if exclude is specified and path matches" in{
      new IncludeExcludeLogic(Nil, List(new Regex("/foo/.*")))("/foo/bar") must be_==(false)
      new IncludeExcludeLogic(Nil, List(new Regex("/foo/bar")))("/foo/bar") must be_==(false)
    }
    "include if exclude and incude are specified and path match to both settings" in{
      new IncludeExcludeLogic(List(new Regex("/foo/.*")), List(new Regex("/foo/.*")))("/foo/bar") must be_==(true)
    }

  }
}