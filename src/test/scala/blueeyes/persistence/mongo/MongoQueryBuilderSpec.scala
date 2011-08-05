package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryBuilder._
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import scala.collection.immutable.ListSet

class MongoQueryBuilderSpec  extends Specification{
  private val jObject = JObject(JField("Foo", JString("bar")) :: Nil)

  "creates select query" in{
    select("foo", "bar").from("collection") mustEqual ( MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection") )
  }
  "creates group query" in{
    group(JObject(Nil), "dummy", "foo", "bar").from("collection") mustEqual ( MongoGroupQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", JObject(Nil), "dummy") )
  }
  "creates mapReduce query" in{
    mapReduce("foo", "bar").from("collection") mustEqual ( MongoMapReduceQuery("foo", "bar",  "collection") )
  }
  "creates distinct query" in{
    distinct("foo").from("collection") mustEqual ( MongoDistinctQuery(JPath("foo"), "collection") )
  }
  "creates selectOne query" in{
    selectOne("foo", "bar").from("collection") mustEqual ( MongoSelectOneQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection") )
  }
  "creates remove query" in{
    remove.from("collection") mustEqual ( MongoRemoveQuery("collection") )
  }
  "creates count query" in{
    count.from("collection") mustEqual ( MongoCountQuery("collection") )
  }
  "creates insert query" in{
    insert(jObject).into("collection") mustEqual ( MongoInsertQuery("collection", jObject :: Nil) )
  }
  "creates ensureIndex query" in{
    ensureIndex("index").on("address.city").in("collection") mustEqual ( MongoEnsureIndexQuery("collection", "index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), false) )
  }
  "creates dropIndex query" in{
    dropIndex("index").in("collection") mustEqual ( MongoDropIndexQuery("collection", "index") )
  }
  "creates dropIndexes query" in{
    dropIndexes.in("collection") mustEqual ( MongoDropIndexesQuery("collection") )
  }
  "creates ensureUniqueIndex query" in{
    ensureUniqueIndex("index").on("address.city").in("collection") mustEqual ( MongoEnsureIndexQuery("collection", "index", ListSet.empty[Tuple2[JPath, IndexType]] + Tuple2(JPath("address.city"), OrdinaryIndex), true) )
  }
  "creates update query" in{
    update("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject) )
  }
  "creates updateMany query" in{
    updateMany("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, false, true) )
  }
  "creates upsert query" in{
    upsert("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, true, false) )
  }
  "creates upsertMany query" in{
    upsertMany("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, true, true) )
  } 
}
