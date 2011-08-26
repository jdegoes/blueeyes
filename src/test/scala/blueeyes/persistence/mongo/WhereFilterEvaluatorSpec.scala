package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class WhereFilterEvaluatorSpec extends Specification{
  private val jObject      = JObject(JField("address", JObject( JField("code", JInt(1)) :: Nil)) :: Nil)

  "WhereFilterEvaluator" should{
    "return true when script uses 'obj' and return true" in{
      val function = "function(){return obj.address.code == 1;}"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (true)
    }
    "return true when script uses 'this' and return true" in{
      val function = "function(){return this.address.code == 1;}"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (true)
    }
    "return true when 'function' is missing" in{
      val function = "{return this.address.code == 1;}"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (true)
    }
    "return true when 'function' and 'brace' are missing" in{
      val function = "return this.address.code == 1"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (true)
    }
    "return true when 'function', 'brace' and 'return' are missing" in{
      val function = "this.address.code == 1"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (true)
    }
    "return false when script uses 'obj' and return true" in{
      val function = "function(){return obj.address.code == 2;}"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (false)
    }
    "return false when script uses 'this' and return true" in{
      val function = "function(){return this.address.code == 2;}"

      WhereFilterEvaluator(jObject, whereObject(function)) must be (false)
    }
    "return false when field is missing" in{
      val function = "function(){return obj.address.street == 2;}"
      WhereFilterEvaluator(jObject, whereObject(function)) must be (false)
    }
  }

  private def whereObject(function: String) = JString(function)

}