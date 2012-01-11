package blueeyes.persistence.mongo.mock

import org.specs2.mutable.Specification
import blueeyes.persistence.mongo._
import blueeyes.json.JPath
import blueeyes.json.JsonAST._

class JObjectOrderingFactorySpec extends Specification{
  private val nearFilter = MongoFieldFilter("foo", MongoFilterOperators.$near, JObject(List(JField("$near", JArray(List(JDouble(1.0), JDouble(2.0)))))))
  private val nearSphereFilter = MongoFieldFilter("foo", MongoFilterOperators.$nearSphere, JObject(List(JField("$nearSphere", JArray(List(JDouble(1.0), JDouble(2.0)))))))
  "JObjectOrderingFactory" should{
    "creates JObjectOrdering when sort is defined" in{
      JObjectOrderingFactory(None, Some(JPath("foo") >> )) must beSome(JObjectOrdering("foo", 1))
    }
    "creates GeoFieldOrdering when only near filter is defined" in{
      JObjectOrderingFactory(Some(nearFilter), None) must beSome(GeoFieldOrdering("foo", 1.0, 2.0))
    }
    "creates GeoFieldOrdering when only nearSphere filter is defined" in{
      JObjectOrderingFactory(Some(nearSphereFilter), None) must beSome(GeoFieldOrdering("foo", 1.0, 2.0))
    }
  }
}