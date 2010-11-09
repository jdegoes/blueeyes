package blueeyes.js

import org.specs.Specification

class RhinoScriptSpec extends Specification with RhinoScriptImplicits{
  "execute script" in{
    val parameters = Map("prev" -> 1, "obj" -> 3)
    
    val result = RhinoScript("{ prev += obj; }")(parameters)
    
    result mustEqual(4)
  }
}