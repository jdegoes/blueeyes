package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import Evaluators._

class RegexFilterEvaluatorSpec extends Specification{
  "RegexFilterEvaluator" should{
    "return true when regex (without options) is matched" in {
      val regex = JObject(List(JField("$regex", JString("f..")), JField("$options", JString(""))))
      RegexFilterEvaluator(JString("foo"), regex) must be_==(true)
    }
    "return false  when regex (without options) is not matched" in {
      val regex = JObject(List(JField("$regex", JString("f..")), JField("$options", JString(""))))
      RegexFilterEvaluator(JString("foo foo"), regex) must be_==(true)
    }
    "support Case insensitive option" in {
      val regex = JObject(List(JField("$regex", JString("F..")), JField("$options", JString(""))))
      RegexFilterEvaluator(JString("foo"), regex) must be_==(false)

      val regex1 = JObject(List(JField("$regex", JString("F..")), JField("$options", JString("i"))))
      RegexFilterEvaluator(JString("foo"), regex1) must be_==(true)
    }
  }
}