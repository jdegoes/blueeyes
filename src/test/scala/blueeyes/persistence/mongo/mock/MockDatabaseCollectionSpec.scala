package blueeyes.persistence.mongo.mock

import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.persistence.mongo.MongoFilterOperators._
import com.mongodb.MongoException
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.{JsonParser, JPath}
import scala.collection.immutable.ListSet

import scalaz._
import Scalaz._

class MockDatabaseCollectionSpec extends Specification{
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: JField("location", JArray(List(JInt(40), JInt(40)))) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: JField("location", JArray(List(JInt(40), JInt(40)))) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("3")) ::  Nil)) :: JField("location", JArray(List(JInt(40), JInt(40)))) :: Nil)
  private val jObject3 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("4")) ::  Nil)) :: Nil)
//  private val jObjectWithArray = JObject(JField("array", JArray( JString("C") :: Nil)) :: Nil)
  private val jobjects = jObject :: jObject1 :: jObject2 :: jObject3 :: Nil

  private val jobjectsWithArray = parse("""{ "foo" : [{"shape" : "square", "color" : "purple", "thick" : false}, {"shape" : "circle", "color" : "red", "thick" : true}] } """) :: parse("""{ "foo" : [{"shape" : "square", "color" : "red", "thick" : true}, {"shape" : "circle", "color" : "purple", "thick" : false}] }""") :: Nil
  private val unsetTest = parse("""{ "channelId" : "foo", "feeds" { "bar" : {"field" : "one" }, "baz" : {"field" : "one" }  } }""")

  private val sort     = MongoSort("address.street", MongoSortOrderDescending)

  private def parse(value: String) = JsonParser.parse(value).asInstanceOf[JObject]

  "mapReduce objects" in{
    val objects = parse("""{ "id" : 1, "tags" : ["dog", "cat"] }""") :: parse("""{ "id" : 2, "tags" : ["cat"] }""") :: parse("""{ "id" : 3, "tags" : ["mouse", "cat", "dog"] }""") :: parse("""{ "id" : 4, "tags" : []  }""") :: Nil
    val map     = """function(){ this.tags.forEach( function(z){ emit( z , { count : 1 } ); } );};"""
    val reduce  = """function( key , values ){ var total = 0; for ( var i=0; i<values.length; i++ ) total += values[i].count; return { count : total }; };"""

    val collection = newCollection
    collection.insert(objects)

    val result = collection.mapReduce(map, reduce, None, None)

    val outputCollection = result.outputCollection.collection
    outputCollection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(JObject(List(JField("count",JDouble(2.0)))) :: JObject(List(JField("count",JDouble(3.0)))) :: JObject(List(JField("count",JDouble(1.0)))) :: Nil)
  }

  "group objects" in{
    val objects = parse("""{"address":{ "city":"A", "code":2, "street":"1"  } }""") :: parse("""{"address":{ "city":"A", "code":5, "street":"3"  } }""") :: parse("""{"address":{ "city":"C", "street":"1"  } }""") :: parse("""{"address":{ "code":3, "street":"1"  } }""") :: Nil
    val initial = parse("""{ "csum": 10.0 }""")

    val collection = newCollection
    collection.insert(objects)

    val result = collection.group(MongoSelection(Set(JPath("address.city"))), None, initial, "function(obj,prev) { prev.csum += obj.address.code }")

    result.elements.size must be (3)
    result.elements.contains(parse("""{"address.city":null,"csum":13.0} """)) must be (true)
    result.elements.contains(parse("""{"address.city":"A","csum":17.0} """))  must be (true)

    val withNaN = result.elements.filter(v => v.asInstanceOf[JObject].fields.head == JField("address.city", JString("C"))).head.asInstanceOf[JObject]
    withNaN.fields.tail.head.value.asInstanceOf[JDouble].value.isNaN() must be (true)
  }

  "store jobjects" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jobjects)
  }
  "distinct simple fields" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.distinct("address.city", None) mustEqual(JString("A") :: JString("B") :: JString("C") :: Nil)
  }

  "does not store jobject when unique index exists and objects are the same" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex) + Tuple2(JPath("address.street"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject :: jObject :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
  }
  "store jobject when index is dropped and objects are the same" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex) + Tuple2(JPath("address.street"), OrdinaryIndex), true, JObject(Nil))
    collection.dropIndex("index")
    collection.insert(jObject :: jObject :: Nil)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject :: jObject :: Nil)
  }
  "store jobject when indexes are dropped and objects are the same" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex) + Tuple2(JPath("address.street"), OrdinaryIndex), true, JObject(Nil))
    collection.dropIndexes()
    collection.insert(jObject :: jObject :: Nil)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject :: jObject :: Nil)
  }
  "does not store jobject when Geospatial unique index exists and the same object exists" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("location"), GeospatialIndex), true, JObject(Nil))
    collection.insert(jObject :: Nil)
    collection.insert(jObject1 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject :: Nil)
  }
//  "does not store jobject when Geospatial unique index exists and geo field is out of range" in{
//    val collection = newCollection
//
//    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("location"), GeospatialIndex), true, JObject(Nil))
//    collection.insert(JObject(List(JField("location", JArray(List(JInt(-181), JInt(40)))))) :: Nil)  must throwA[MongoException]
//
//    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
//  }
//  "does not store jobject when Geospatial unique index exists and geo field has 1 element" in{
//    val collection = newCollection
//
//    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("location"), GeospatialIndex), true, JObject(Nil))
//    collection.insert(JObject(List(JField("location", JArray(List(JInt(40)))))) :: Nil)  must throwA[MongoException]
//
//    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
//  }
  "does not store jobject when unique index exists and the same object exists" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)
    collection.insert(jObject2 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject1 :: Nil)
  }
  "does not store jobject when unique index exists and the same object exists" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)
    collection.insert(jObject2 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject1 :: Nil)
  }
  "does not throw an error when the hint refers to not a existing index (by name)" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)
    collection.insert(jObject2 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, Some(NamedHint("index")), false).toList mustEqual(jObject1 :: Nil)
  }
  "does not throw an error when the hint refers to not a existing index (by keys)" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)
    collection.insert(jObject2 :: Nil) must throwA[MongoException]

    collection.select(MongoSelection(Set()), None, None, None, None, Some(KeyedHint(ListSet.empty + JPath("address.city"))), false).toList mustEqual(jObject1 :: Nil)
  }
  "throw an error when hint refers to not existing index (by name)" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)

    collection.select(MongoSelection(Set()), None, None, None, None, Some(NamedHint("foo")), false) must throwA[MongoException]
  }
  "throw an error when hint refers to not existing index (by keys)" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject1 :: Nil)

    collection.select(MongoSelection(Set()), None, None, None, None, Some(KeyedHint(ListSet.empty + JPath("address"))), false) must throwA[MongoException]
  }
  "store jobject when unique index exists and objects are different" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex) + Tuple2(JPath("address.street"), OrdinaryIndex), true, JObject(Nil))
    collection.insert(jObject :: jObject1 :: Nil)
    collection.insert(jObject2 :: jObject3 :: Nil)

    collection.select(MongoSelection(Set()), None, Some(sort), None, None, None, false).toList mustEqual(jobjects.reverse.toStream)
  }
  "update jobject field" in{
    val collection = newCollection

    collection.insert(jObject1 :: Nil)
    collection.update(None, "address.street" set ("3"), false, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual((jObject2 :: Nil).toStream)
  }
  "update not existing jobject field" in{
    val collection = newCollection

    collection.insert(jObject1 :: Nil)
    collection.update(None, "name" set ("foo"), false, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject1.merge(JObject(JField("name", JString("foo")) :: Nil)) :: Nil)
  }
  "update jobject fields" in{
    val collection = newCollection

    collection.insert(jObject :: Nil)
    collection.update(None, ("address.city" set ("B")) |+| ("address.street" set ("3")), false, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual((jObject2 :: Nil).toStream)
  }
  "pull jobject by field query" in{
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.update(None, ("foo" pull ("shape" === "square")), false, true)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual((JsonParser.parse("""{ "foo" : [
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
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.update(None, ("foo" pull (("shape" === "square") && ("color" === "purple")).elemMatch("")), false, true)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual((JsonParser.parse("""{ "foo" : [
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
    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(None, jObject2, false, true)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject2 :: jObject2:: Nil)
  }
  "update object by filter" in{
    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(Some(MongoFieldFilter("address.city", $eq,"A")), jObject2, false, true)

    collection.select(MongoSelection(Set()), None, Some(sort), None, None, None, false).toList mustEqual(jObject2 :: jObject1  :: Nil)
  }
  "does not update object by MongoUpdateNothing" in{
    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(Some(MongoFieldFilter("address.city", $eq,"A")), MongoUpdateNothing, false, true)

    collection.select(MongoSelection(Set()), None, Some(sort), None, None, None, false).toList mustEqual(jObject1  :: jObject :: Nil)
  }
  "update only one object when multi is false" in{
    val collection = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    collection.update(None, MongoUpdateObject(jObject2), false, false)

    val result = collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList
    result.contains(jObject2) must be (true)
    (if (result.contains(jObject)) !result.contains(jObject1) else result.contains(jObject1)) must be (true)
  }
  "insert by update when upsert is true" in{
    val collection = newCollection

    collection.update(None, MongoUpdateObject(jObject2), true, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject2 :: Nil)
  }
  "update using uset" in{

    val collection = newCollection

    collection.insert(unsetTest :: Nil)

    collection.update(Some("channelId" === "foo"), JPath(".feeds.bar") unset, false, false)

    val feeds = collection.select(MongoSelection(Set()), None, None, None, None, None, false).head \\ "feeds"

    feeds.asInstanceOf[JObject].fields.length mustEqual (1)
  }
  "insert by update field when upsert is true" in{
    val collection = newCollection

    collection.update(None, "foo".inc(1) , true, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(JObject(JField("foo", JInt(1)) :: Nil) :: Nil)
  }
  "insert filter values by update field when upsert is true" in{
    val collection = newCollection

    collection.update(Some(("bar" === 1) & ("my.name" === "Michael")), "foo".inc(1) , true, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(JObject(JField("foo", JInt(1)) :: JField("my", JObject(JField("name", JString("Michael")) :: Nil)) :: JField("bar", JInt(1)) :: Nil) :: Nil)
  }
  "does insert filter values by update object when upsert is true" in{
    val collection = newCollection

    collection.update(Some("bar" === "foo"), MongoUpdateObject(JObject(JField("baz", JString("foo")) :: Nil)) , true, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(JObject(JField("baz", JString("foo")) :: Nil) :: Nil)
  }
  "does insert by update field when upsert is false" in{
    val collection = newCollection

    collection.update(None, "foo".inc(1) , false, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
  }
  "update by update when upsert is true and index exist" in{
    val collection = newCollection

    collection.ensureIndex("index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true, JObject(Nil))
    collection.update(None, MongoUpdateObject(jObject2), true, false)
    collection.update(None, MongoUpdateObject(jObject2), true, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject2 :: Nil)
  }

  // Added by josh
  /*
  "upsert should work" in {
    val collection = newCollection
    upsert(collection)
    collection.select(MongoSelection(Set()), None, None, None, None) mustEqual(jObject2 :: Nil)
  }
  */

  "does not insert by update when upsert is false" in{
    val collection = newCollection

    collection.update(None, MongoUpdateObject(jObject2), false, false)

    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
  }
  "removes all jobjects when filter is not specified" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(None)
    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(Nil)
  }
  "count all jobjects when filter is not specified" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.count(None) mustEqual(4)
  }
  "count jobjects which match filter" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.count(Some(MongoFieldFilter("address.city", $eq,"A"))) mustEqual(1)
  }
  "removes jobjects which match filter" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.remove(Some(MongoFieldFilter("address.city", $eq,"A")))
    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jObject1 :: jObject2 :: jObject3 :: Nil)
  }
  "select all jobjects when filter is not specified" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), None, None, None, None, None, false).toList mustEqual(jobjects)
  }
  "select jobjects by filter" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("address.city", $eq,"A")), None, None, None, None, false).toList mustEqual(jObject :: Nil)
  }
  "select jobjects by filter with near filter" in{
    val collection = newCollection

    collection.insert(jObject :: jObject3 :: Nil)
    val select = collection.select(MongoSelection(Set()), Some(MongoFieldFilter("location", $near, JObject(JField("$near", JArray(List(JDouble(1.0), JDouble(2.0)))) :: Nil))), None, None, None, None, false).toList
    select mustEqual(jObject :: Nil)
  }
  "select jobjects by filter with exists" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), Some(MongoAndFilter(ListSet.empty[MongoFilter] + MongoFieldFilter("address.city", $eq, "A") + MongoFieldFilter("address.street", $exists, true))), None, None, None, None, false).toList mustEqual(jObject :: Nil)
  }
  "does not select jobjects by filter with wrong exists" in{
    val collection = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("address.data", $exists, true)), None, None, None, None, false).toList mustEqual(Nil)
  }
//  "does not select jobjects by filter with index in path" in{
//    val collection = newCollection
//
//    collection.insert(jobjects)
//    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("array[1]", $eq, true)), None, None, None, false).toList mustEqual(Nil)
//  }
  "select jobjects with array when array element field filter is specified" in{
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("foo.shape", $eq,"square") && MongoFieldFilter("foo.color", $eq,"purple")), None, None, None, None, false).toList mustEqual(jobjectsWithArray)
  }
  "select jobjects with array when array element filter is specified" in{
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    val filterObject = JsonParser.parse(""" {"shape" : "square", "color" : "purple","thick" : false} """).asInstanceOf[JObject]
    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("foo", $eq, filterObject)), None, None, None, None, false).toList mustEqual(List(jobjectsWithArray.head).toStream)
  }
  "select jobjects with array when elemMatch filter is specified" in{
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    collection.select(MongoSelection(Set()), Some(MongoElementsMatchFilter("foo", (MongoFieldFilter("shape", $eq,"square") && MongoFieldFilter("color", $eq,"red")))), None, None, None, None, false).toList mustEqual(List(jobjectsWithArray.tail.head).toStream)
  }

  "does not select jobjects with array when wrong array element filter is specified" in{
    val collection = newCollection

    collection.insert(jobjectsWithArray)
    val filterObject = JsonParser.parse(""" {"shape" : "square", "color" : "purple"} """).asInstanceOf[JObject]
    collection.select(MongoSelection(Set()), Some(MongoFieldFilter("foo", $eq, filterObject)), None, None, None, None, false).toList mustEqual(Nil.toStream)
  }
  "select ordered objects" in{
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), None, Some(sort), None, None, None, false).toList mustEqual(jobjects.reverse)
  }
  "skip objects" in{
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), None, Some(sort), Some(2), None, None, false).toList mustEqual(jObject1 :: jObject:: Nil)
  }
  "limit objects" in{
    val collection  = newCollection

    collection.insert(jobjects)
    collection.select(MongoSelection(Set()), None, Some(sort), Some(2), Some(1), None, false).toList mustEqual(jObject1:: Nil)
  }

  "select specified objects fields" in{
    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(Set(JPath("address.city"))), None, Some(sort), None, None, None, false).toList mustEqual(fields1 :: fields :: Nil)
  }
  "select specified objects fields when some fields are missing" in{
    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(Set(JPath("address.city"), JPath("address.phone"))), None, Some(sort), None, None, None, false).toList mustEqual(fields1 :: fields :: Nil)
  }
  "select nothing when wwong selection is specified" in{
    val collection  = newCollection

    collection.insert(jObject :: jObject1 :: Nil)
    val fields  = JObject(JField("address", JObject(JField("city", JString("A")) :: Nil)) :: Nil)
    val fields1 = JObject(JField("address", JObject(JField("city", JString("B")) :: Nil)) :: Nil)
    collection.select(MongoSelection(Set(JPath("address.town"))), None, Some(sort), None, None, None, false).toList mustEqual(Nil)
  }

  private def newCollection = new MockDatabaseCollection("foo", new MockDatabase(new MockMongo()))
}
