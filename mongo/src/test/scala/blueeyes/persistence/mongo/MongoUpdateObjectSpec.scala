package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.{JsonParser, JPath}
import blueeyes.json.JsonAST.JObject
import MongoUpdateObject._

class MongoUpdateObjectSpec  extends Specification {
  "decompose simple JObject" in{
    val jObject = parse("""{ "id" : 1.0, "name" : "foo" }""")
    decompose(jObject).toSet mustEqual(Set(("id" set 1.0), ("name" set "foo")))
  }
  "decompose nested JObject" in{
    val jObject = parse("""{ "id" : 1.0, "name" : { "first" : "foo" } }""")
    decompose(jObject).toSet mustEqual(Set(("id" set 1.0), (JPath("name") \ "first" set "foo")))
  }
  "decompose nested JObject with null element" in{
    val jObject = parse("""{ "id" : 1.0, "name" : null }""")
    decompose(jObject).toSet mustEqual(Set(("id" set 1.0), ("name" set MongoPrimitiveNull)))
  }

  private def parse(value: String) = JsonParser.parse(value).asInstanceOf[JObject]
}