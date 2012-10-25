package blueeyes.persistence.mongo

import blueeyes.json._
import org.specs2.mutable.Specification
import com.mongodb.MongoException

class WhereFilterScriptBuilderSpec extends Specification with Evaluators.WhereFilterScriptBuilder{
  private val scriptPattern = """var obj = %s; obj.evaluate = %s; obj.evaluate()"""
  private val jObject       = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("3")) :: JField("code", JNum(1)) :: Nil)) :: Nil)
  private val json          = jObject.renderCompact

  "WhereFilterScriptBuilder" should{
    "build script when full function is defined" in{
      val function = "function(){return obj.address.city;}"
      build(function, jObject) mustEqual(scriptPattern.format(json, function))
    }
    "build script when 'function' is missing" in{
      val function = "{return obj.address.city;}"
      build(function, jObject) mustEqual(scriptPattern.format(json, "function()" + function))
    }
    "build script when 'function' and 'braces' are missing" in{
      val function = "obj.address.city > 0"
      build(function, jObject) mustEqual(scriptPattern.format(json, "function(){return " + function + "}"))
    }
    "build script when 'function' and 'braces' are missing but 'return' exists" in{
      val function = "return obj.address.city > 0"
      build(function, jObject) mustEqual(scriptPattern.format(json, "function(){" + function + "}"))
    }
    "throw mongo exception when 'function' and 'return' are missing" in{
      val function = "{obj.address.city;}"
      build(function, jObject) must throwAn[MongoException]
    }
  }
}