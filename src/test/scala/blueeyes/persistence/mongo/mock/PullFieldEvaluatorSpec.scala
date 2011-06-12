package blueeyes.persistence.mongo.mock

import scala.collection.immutable.ListSet
import org.specs.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvaluators._
import com.mongodb.MongoException
import blueeyes.json.JsonParser
import blueeyes.persistence.mongo.MongoFilterOperators._
import blueeyes.persistence.mongo._

class PullFieldEvaluatorSpec  extends Specification{

  "pull element by simple filter" in {
    val operation = ("foo" pull ("" === 1)).asInstanceOf[MongoUpdateField]
     PullFieldEvaluator(JsonParser.parse("[1, 2]"), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }

  "pull element by by complex filter " in {
    val operation = ("foo" pull ("foo" === 1)).asInstanceOf[MongoUpdateField]
     PullFieldEvaluator(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]"""), operation.filter) mustEqual(JsonParser.parse("""[{"foo": 2}]"""))
  }
  "pull element by element match " in {
    val operation = ("foo" pull (MongoAndFilter(ListSet.empty + MongoFieldFilter("foo", $eq, 1)).elemMatch(""))).asInstanceOf[MongoUpdateField]
     PullFieldEvaluator(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]"""), operation.filter) mustEqual(JsonParser.parse("""[{"foo": 2}]"""))
  }
  "cannot pull from not Array field" in {
    val operation = ("foo" pull ("" === 3)).asInstanceOf[MongoUpdateField]
    PullFieldEvaluator(JInt(2), operation.filter) must throwA[MongoException]
  }
}