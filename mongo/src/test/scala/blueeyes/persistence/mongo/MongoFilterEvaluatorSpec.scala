package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import blueeyes.persistence.mongo.MongoFilterOperators._
import blueeyes.json.JParser

class MongoFilterEvaluatorSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  private val jobjectsWithArray = JParser.parse("""{ "foo" : [
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
] } """) :: JParser.parse("""
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


  "MongoFilterEvaluator" should{
    "selects all objects by MongoFilterAll" in{
      MongoFilterEvaluator(jobjects).filter(MongoFilterAll) mustEqual(jobjects)
    }
    "selects objects by field query" in{
      MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B")) mustEqual(jObject1 :: jObject2 :: Nil)
    }
    "selects objects by 'or' query" in{
      val result = jObject1 :: jObject2 :: jObject3 :: Nil
      MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B") || MongoFieldFilter("street.street", $eq,"4")).filterNot (result contains) mustEqual(Nil)
    }
    "selects objects by 'and' query" in{
      val result = jObject2 :: Nil
      MongoFilterEvaluator(jobjects).filter(MongoFieldFilter("address.city", $eq,"B") && MongoFieldFilter("street.street", $eq,"3")).filterNot (result contains) mustEqual(Nil)
    }

    "select object using elemeMatch" in {
       MongoFilterEvaluator(jobjectsWithArray).filter(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"purple")))) mustEqual(jobjectsWithArray.head :: Nil)
    }
    "does not select object using elemeMatch and wrong query" in {
       MongoFilterEvaluator(jobjectsWithArray).filter(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"freen")))) mustEqual(Nil)
    }
    "select element from array" in {
       MongoFilterEvaluator(JParser.parse("[1, 2]").asInstanceOf[JArray].elements).filter(MongoFieldFilter("", $eq, 1)) mustEqual(JNum(1) :: Nil)
    }

    "select element by complex filter " in {
       MongoFilterEvaluator(JParser.parse("""[{"foo": 1}, {"foo": 2}]""").asInstanceOf[JArray].elements).filter(MongoFieldFilter("foo", $eq, 1)) mustEqual(JParser.parse("""{"foo": 1}""") :: Nil)
    }
    "select element from array by element match " in {
       MongoFilterEvaluator(JParser.parse("""[{"foo": 1}, {"foo": 2}]""").asInstanceOf[JArray].elements).filter(MongoAndFilter(List(MongoFieldFilter("foo", $eq, 1))).elemMatch("")) mustEqual(JParser.parse("""{"foo": 1}""") :: Nil)
    }
  }
}