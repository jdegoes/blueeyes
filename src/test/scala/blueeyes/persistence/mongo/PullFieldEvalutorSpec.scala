package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import MongoImplicits._
import MongoFilterOperators._
import com.mongodb.MongoException
import blueeyes.json.JsonParser

class PullFieldEvalutorSpec  extends Specification{

  "pull element by simple filter" in {
    import MongoFilterImplicits._
    val operation = "foo" pull ("" === 1)
     PullFieldEvalutor(JsonParser.parse("[1, 2]"), operation.filter) mustEqual(JArray(JInt(2) :: Nil))
  }

  "pull element by by complex filter " in {
    import MongoFilterImplicits._
    val operation = "foo" pull ("foo" === 1)
     PullFieldEvalutor(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]"""), operation.filter) mustEqual(JsonParser.parse("""[{"foo": 2}]"""))
  }
  "pull element by element match " in {
    import MongoFilterImplicits._
    val operation = "foo" pull (MongoAndFilter(MongoFieldFilter("foo", $eq, 1) :: Nil).elemMatch(""))
     PullFieldEvalutor(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]"""), operation.filter) mustEqual(JsonParser.parse("""[{"foo": 2}]"""))
  }
  "cannot pull from not Array field" in {
    import MongoFilterImplicits._

    val operation = "foo" pull ("" === 3)
    PullFieldEvalutor(JInt(2), operation.filter) must throwA[MongoException]
  }
}