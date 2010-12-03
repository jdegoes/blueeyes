package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.MongoFilterOperators._

//private[mock] object MockMongoFiltersImplementation{
//  object JObjectsFilter{
//    def apply(jobjects: List[JValue], filter: MongoFilter):  List[JValue] = filter match{
//      case MongoFilterAll               => jobjects
//      case x : MongoFieldFilter         => MongoFieldFilterEvaluator(jobjects, x)
//      case x : MongoOrFilter            => MongoOrFilterEvaluator   (jobjects, x)
//      case x : MongoAndFilter           => MongoAndFilterEvaluator  (jobjects, x)
//      case x : MongoElementsMatchFilter => MongoElementsMatchFilterEvaluator(jobjects, x)
//    }
//  }
//
//
//}