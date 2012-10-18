package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import Evaluators._

class NinFieldFilterEvaluatorSpec extends Specification {
  "returns false when value in array" in {
    NinFieldFilterEvaluator(JString("b"), JArray(JString("b") :: JString("a") :: Nil)) must be_==(false)
  }
  "returns true when value in not array" in {
    NinFieldFilterEvaluator(JString("b"), JArray(JString("c") :: JString("a") :: Nil)) must be_==(true)
  }
}