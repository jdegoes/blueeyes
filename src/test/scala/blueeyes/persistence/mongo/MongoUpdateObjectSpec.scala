package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.{JsonParser, JPath}
import blueeyes.json.JsonAST.JObject
import MongoUpdateObject._

class MongoUpdateObjectSpec  extends Specification {
  "decompose simple JObject" in{
    import MongoImplicits._

    val jObject = parse("""{ "id" : 1.0, "name" : "foo" }""")
    decompose(jObject) mustEqual(List(("id" set 1.0), ("name" set "foo")))
  }
  "decompose nested JObject" in{
    import MongoImplicits._

    val jObject = parse("""{ "id" : 1.0, "name" : { "first" : "foo" } }""")
    decompose(jObject) mustEqual(List(("id" set 1.0), (JPath("name") \ "first" set "foo")))
  }
  "decompose nested JObject with null element" in{
    import MongoImplicits._

    val jObject = parse("""{ "id" : 1.0, "name" : null }""")
    decompose(jObject) mustEqual(List(("id" set 1.0), ("name" unset)))
  }

  private def parse(value: String) = JsonParser.parse(value).asInstanceOf[JObject]
}