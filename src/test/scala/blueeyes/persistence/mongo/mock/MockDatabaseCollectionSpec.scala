package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.persistence.mongo.MongoFilterOperators._
import com.mongodb.MongoException
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.{JsonParser, JPath}

class MockDatabaseCollectionSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  private val jobjectsWithArray = parse("""{ "foo" : [{"shape" : "square", "color" : "purple", "thick" : false}, {"shape" : "circle", "color" : "red", "thick" : true}] } """) :: parse("""{ "foo" : [{"shape" : "square", "color" : "red", "thick" : true}, {"shape" : "circle", "color" : "purple", "thick" : false}] }""") :: Nil

  private val sort     = MongoSort("address.street", MongoSortOrderDescending)

  private def parse(value: String) = JsonParser.parse(value).asInstanceOf[JObject]

  "mapReduce objects" in{
    import MongoImplicits._

    val objects = parse("""{ "id" : 1, "tags" : ["dog", "cat"] }""") :: parse("""{ "id" : 2, "tags" : ["cat"] }""") :: parse("""{ "id" : 3, "tags" : ["mouse", "cat", "dog"] }""") :: parse("""{ "id" : 4, "tags" : []  }""") :: Nil
    val map     = """function(){ this.tags.forEach( function(z){ emit( z , { count : 1 } ); } );};"""
    val reduce  = """function( key , values ){ var total = 0; for ( var i=0; i<values.length; i++ ) total += values[i].count; return { count : total }; };"""

    val collection = newCollection
    collection.insert(objects)

    val result = collection.mapReduce(map, reduce, None, None)

    val outputCollection = result.outpotCollection.collection
    outputCollection.select(MongoSelection(Nil), None, None, None, None) mustEqual(JObject(List(JField("count",JDouble(2.0)))) :: JObject(List(JField("count",JDouble(3.0)))) :: JObject(List(JField("count",JDouble(1.0)))) :: Nil)
  }


  "group objects" in{
    import MongoImplicits._

    val objects = parse("""{"address":{ "city":"A", "code":2, "street":"1"  } }""") :: parse("""{"address":{ "city":"A", "code":5, "street":"3"  } }""") :: parse("""{"address":{ "city":"C", "street":"1"  } }""") :: parse("""{"address":{ "code":3, "street":"1"  } }""") :: Nil
    val initial = parse("""{ "csum": 10.0 }""")

    val collection = newCollection
    collection.insert(objects)

    val result = collection.group(MongoSelection(JPath("address.city") :: Nil), None, initial, "function(obj,prev) { prev.csum += obj.address.code }")

    result.elements.size must be (3)
    result.elements.contains(parse("""{"address.city":null,"csum":13.0} """)) must be (true)
    result.elements.contains(parse("""{"address.city":"A","csum":17.0} """))  must be (true)

    val withNaN = result.elements.filter(v => v.asInstanceOf[JObject].fields.head == JField("address.city", JString("C"))).head.asInstanceOf[JObject]
    withNaN.fields.tail.head.value.asInstanceOf[JDouble].value.isNaN() must be (true)
  }

  "store jobjects" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobjects)
  }
  "distinct simple fields" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.distinct("address.city", None) mustEqual(JString("A") :: JString("B") :: JString("C") :: Nil)
  }

  "does not store jobject when unique index exists and objects are the same" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)
    collection.insert(jObject :: jObject :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(Nil)
  }
  "store jobject when index is dropped and objects are the same" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)
    collection.dropIndex("index")
    collection.insert(jObject :: jObject :: Nil)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject :: jObject :: Nil)
  }
  "store jobject when indexes are dropped and objects are the same" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)
    collection.dropIndexes
    collection.insert(jObject :: jObject :: Nil)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject :: jObject :: Nil)
  }
  "does not store jobject when unique index exists and the same object exists" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: Nil, true)
    collection.insert(jObject1 :: Nil)
    collection.insert(jObject2 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject1 :: Nil)
  }
  "store jobject when unique index exists and objects are different" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)
    collection.insert(jObject :: jObject1 :: Nil)
    collection.insert(jObject2 :: jObject3 :: Nil)

    collection.select(MongoSelection(Nil), None, Some(sort), None, None) mustEqual(jobjects.reverse.toStream)
  }
  "update jobject field" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject1 :: Nil)
    collection.update(None, "address.street" set ("3"), false, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual((jObject2 :: Nil).toStream)
  }
  "update not existing jobject field" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject1 :: Nil)
    collection.update(None, "name" set ("foo"), false, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual((jObject1.merge(JObject(JField("name", JString("foo")) :: Nil)) :: Nil).toStream)
  }
  "update jobject fields" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject :: Nil)
    collection.update(None, ("address.city" set ("B")) & ("address.street" set ("3")), false, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual((jObject2 :: Nil).toStream)
  }
  "pull jobject by field query" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.update(None, ("foo" pull ("shape" === "square")), false, true)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual((JsonParser.parse("""{ "foo" : [
      {
        "shape" : "circle",
        "color" : "red",
        "thick" : true
      }
] } """).asInstanceOf[JObject] :: JsonParser.parse("""
{ "foo" : [
      {
        "shape" : "circle",
        "color" : "purple",
        "thick" : false
      }
] }""").asInstanceOf[JObject] :: Nil).toStream)
  }

  "pull jobject by element match" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.update(None, ("foo" pull (("shape" === "square") && ("color" === "purple")).elemMatch("")), false, true)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual((JsonParser.parse("""{ "foo" : [
      {
        "shape" : "circle",
        "color" : "red",
        "thick" : true
      }
] } """).asInstanceOf[JObject] :: JsonParser.parse("""
{ "foo" : [
      {
        "shape" : "square",
        "color" : "red",
        "thick" : true
      },
      {
        "shape" : "circle",
        "color" : "purple",
        "thick" : false
      }
] }""").asInstanceOf[JObject] :: Nil).toStream)
  }
  "update all objects" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(None, jObject2, false, true)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject2 :: jObject2:: Nil)
  }
  "update object by filter" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(Some(MongoFieldFilter("address.city", $eq,"A")), jObject2, false, true)

    collection.select(MongoSelection(Nil), None, Some(sort), None, None) mustEqual(jObject2 :: jObject1  :: Nil)
  }
  "does not update object by MongoUpdateNothing" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(Some(MongoFieldFilter("address.city", $eq,"A")), MongoUpdateNothing, false, true)

    collection.select(MongoSelection(Nil), None, Some(sort), None, None) mustEqual(jObject1  :: jObject :: Nil)
  }
  "update only one object when multi is false" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(None, MongoUpdateObject(jObject2), false, false)

    val result = collection.select(MongoSelection(Nil), None, None, None, None)
    result.contains(jObject2) must be (true)
    (if (result.contains(jObject)) !result.contains(jObject1) else result.contains(jObject1)) must be (true)
  }
  "insert by update when upsert is true" in{
    import MongoImplicits._

    val collection = newCollection

    collection.update(None, MongoUpdateObject(jObject2), true, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject2 :: Nil)
  }
  "update by update when upsert is true and index exist" in{
    import MongoImplicits._

    val collection = newCollection

    collection.ensureIndex("index", JPath("address.city") :: Nil, true)
    collection.update(None, MongoUpdateObject(jObject2), true, false)
    collection.update(None, MongoUpdateObject(jObject2), true, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject2 :: Nil)
  }

  // Added by josh
  /*
  "upsert should work" in {
    val collection = newCollection
    upsert(collection) 
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject2 :: Nil)
  } 
  */

  "does not insert by update when upsert is false" in{
    import MongoImplicits._

    val collection = newCollection

    collection.update(None, MongoUpdateObject(jObject2), false, false)

    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(Nil)
  }
  "removes all jobjects when filter is not specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(None)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(Nil)
  }
  "count all jobjects when filter is not specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.count(None) mustEqual(4)
  }
  "count jobjects which match filter" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.count(Some(MongoFieldFilter("address.city", $eq,"A"))) mustEqual(1)
  }
  "removes jobjects which match filter" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(Some(MongoFieldFilter("address.city", $eq,"A")))
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jObject1 :: jObject2 :: jObject3 :: Nil)
  }
  "select all jobjects when filter is not specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, None, None, None) mustEqual(jobjects)
  }
  "select jobjects by filter" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("address.city", $eq,"A")), None, None, None) mustEqual(jObject :: Nil)
  }
  "select jobjects with array when array element field filter is specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("foo.shape", $eq,"square") && MongoFieldFilter("foo.color", $eq,"purple")), None, None, None) mustEqual(jobjectsWithArray)
  }
  "select jobjects with array when array element filter is specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    val filterObject = JsonParser.parse(""" {"shape" : "square", "color" : "purple","thick" : false} """).asInstanceOf[JObject]
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("foo", $eq, filterObject)), None, None, None) mustEqual(List(jobjectsWithArray.head).toStream)
  }
  "select jobjects with array when elemMatch filter is specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.select(MongoSelection(Nil), Some(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"red")))), None, None, None) mustEqual(List(jobjectsWithArray.tail.head).toStream)
  }

  "does not select jobjects with array when wrong array element filter is specified" in{
    import MongoImplicits._

    val collection = newCollection

    collection.insert(jobjectsWithArray)
    val filterObject = JsonParser.parse(""" {"shape" : "square", "color" : "purple"} """).asInstanceOf[JObject]
    collection.select(MongoSelection(Nil), Some(MongoFieldFilter("foo", $eq, filterObject)), None, None, None) mustEqual(Nil.toStream)
  }
  "select ordered objects" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), None, None) mustEqual(jobjects.reverse)
  }
  "skip objects" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), Some(2), None) mustEqual(jObject1 :: jObject:: Nil)
  }
  "limit objects" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Nil), None, Some(sort), Some(2), Some(1)) mustEqual(jObject1:: Nil)
  }

  "select specified objects fields" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(JPath("address.city") :: Nil), None, Some(sort), None, None) mustEqual(fields1 :: fields :: Nil)
  }
  "select specified objects fields when some fields are missing" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(JPath("address.city") :: JPath("address.phone") :: Nil), None, Some(sort), None, None) mustEqual(fields1 :: fields :: Nil)
  }
  "select nothing when wwong selection is specified" in{
    import MongoImplicits._

    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(JPath("address.town") :: Nil), None, Some(sort), None, None) mustEqual(Nil)
  }

  private def newCollection = new MockDatabaseCollection()
}
