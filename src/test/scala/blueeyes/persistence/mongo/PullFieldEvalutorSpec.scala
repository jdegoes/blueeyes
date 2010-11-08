package blueeyes.persistence.mongo

import org.spex.Specification
import blueeyes.json.JsonAST._
import MockMongoUpdateEvalutors._
import blueeyes.json.JPathImplicits._
import MongoImplicits._
import com.mongodb.MongoException

class PullFieldEvalutorSpec  extends Specification{

  "cannot pull from not Array field" in {
    import MongoFilterImplicits._

    val operation = "foo" pull ("" === 3)
    AddToSetFieldEvalutor(JInt(2), operation.filter) must throwA[MongoException]
  }
}