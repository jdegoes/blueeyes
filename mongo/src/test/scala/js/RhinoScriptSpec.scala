package blueeyes.js

import org.specs2.mutable.Specification
import blueeyes.json.{JParser}
import blueeyes.json._

class RhinoScriptSpec extends Specification{
  "RhinoScript" should{
    "execute script which return JObject" in{
      val result = RhinoScript("""var f = function(x){x.foo.bar += 1; return x}; f({foo: {bar: 1}, name: "hello"})""")()

      result mustEqual(Some(JParser.parse("""{"name": "hello", "foo": {"bar": 2.0}}""")))
    }
    "execute script which return simple value" in{
      val result = RhinoScript("""var f = function(){return true}; f()""")()

      result must beSome(JBool(true))
    }
  }
}