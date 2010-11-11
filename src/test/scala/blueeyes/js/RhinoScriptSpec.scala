package blueeyes.js

import org.specs.Specification
import blueeyes.json.{JsonParser}

class RhinoScriptSpec extends Specification{
  "execute pure script" in{
    val result = RhinoScript("""var f = function(x){x.foo.bar += 1; return x}; f({foo: {bar: 1}, name: "hello"})""")()

    result mustEqual(Some(JsonParser.parse("""{"name": "hello", "foo": {"bar": 2.0}}""")))
  }
}

//"""
//    var record  = %s;
//    record.map  = %s
//    record.emit = function(k, v){emitter.emmit(k, v)}
//
//    record.map()"""