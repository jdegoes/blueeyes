package blueeyes.persistence.mongo

import org.specs.Specification
import blueeyes.json.JsonAST._
import blueeyes.persistence.mongo.MongoFilterOperators._
import blueeyes.json.JsonParser

class MongoFilterEvaluatorSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  private val jobjectsWithArray = JsonParser.parse("""{ "foo" : [
      {
        "shape" : "square",
        "color" : "purple",
        "thick" : false
      },
      {
        "shape" : "circle",
        "color" : "red",
        "thick" : true
      }
] } """) :: JsonParser.parse("""
{ "foo" : [
      {
        "shape" : "square",
        "color" : "red",
        "thick" : true
      },
      {
        "shape" : "circle",
        "color" : "green",
        "thick" : false
      }
] }""") :: Nil


  "selects all objects by MongoFilterAll" in{
    import MongoImplicits._
    MongoFilterEvaluator(jobjects).filter(MongoFilterAll) mustEqual(jobjects)
  }
  "selects objects by field query" in{
    import MongoImplicits._
    MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B")) mustEqual(jObject1 :: jObject2 :: Nil)
  }
  "selects objects by 'or' query" in{
    import MongoImplicits._
    val result = jObject1 :: jObject2 :: jObject3 :: Nil
    MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B") || MongoFieldFilter("street.street", $eq,"4")).filterNot (result contains) mustEqual(Nil)
  }
  "selects objects by 'and' query" in{
    import MongoImplicits._
    val result = jObject2 :: Nil
    MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B") && MongoFieldFilter("street.street", $eq,"3")).filterNot (result contains) mustEqual(Nil)
  }

  "select object using elemeMatch" in {
    import MongoImplicits._
     MongoFilterEvaluator(jobjectsWithArray).filter(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"purple")))) mustEqual(jobjectsWithArray.head :: Nil)
  }
  "does not select object using elemeMatch and wrong query" in {
    import MongoImplicits._
     MongoFilterEvaluator(jobjectsWithArray).filter(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"freen")))) mustEqual(Nil)
  }
  "select element from array" in {
    import MongoImplicits._
     MongoFilterEvaluator(JsonParser.parse("[1, 2]").asInstanceOf[JArray].elements).filter(MongoFieldFilter("", $eq, 1)) mustEqual(JInt(1) :: Nil)
  }

  "select element by complex filter " in {
    import MongoImplicits._
     MongoFilterEvaluator(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]""").asInstanceOf[JArray].elements).filter(MongoFieldFilter("foo", $eq, 1)) mustEqual(JsonParser.parse("""{"foo": 1}""") :: Nil)
  }
  "select element from array by element match " in {
    import MongoImplicits._
     MongoFilterEvaluator(JsonParser.parse("""[{"foo": 1}, {"foo": 2}]""").asInstanceOf[JArray].elements).filter(MongoAndFilter(MongoFieldFilter("foo", $eq, 1) :: Nil).elemMatch("")) mustEqual(JsonParser.parse("""{"foo": 1}""") :: Nil)
  }
}