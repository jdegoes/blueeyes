package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import blueeyes.json.JPath
import blueeyes.json.JsonAST._

class MongoQueryBuilderSpec  extends Specification{
  private val jObject = JObject(JField("Foo", JString("bar")) :: Nil)

  "creates select query" in{
    import MongoImplicits._
    select("foo", "bar").from("collection") mustEqual ( MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), "collection") )
  }
  "creates group query" in{
    import MongoImplicits._
    group(JObject(Nil), "dummy", "foo", "bar").from("collection") mustEqual ( MongoGroupQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), "collection", JObject(Nil), "dummy") )
  }
  "creates mapReduce query" in{
    import MongoImplicits._
    mapReduce("foo", "bar").from("collection") mustEqual ( MongoMapReduceQuery("foo", "bar",  "collection") )
  }
  "creates distinct query" in{
    import MongoImplicits._
    distinct("foo").from("collection") mustEqual ( MongoDistinctQuery(JPath("foo"), "collection") )
  }
  "creates selectOne query" in{
    import MongoImplicits._
    selectOne("foo", "bar").from("collection") mustEqual ( MongoSelectOneQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), "collection") )
  }
  "creates remove query" in{
    import MongoImplicits._
    remove.from("collection") mustEqual ( MongoRemoveQuery("collection") )
  }
  "creates count query" in{
    import MongoImplicits._
    count.from("collection") mustEqual ( MongoCountQuery("collection") )
  }
  "creates insert query" in{
    import MongoImplicits._
    insert(jObject).into("collection") mustEqual ( MongoInsertQuery("collection", jObject :: Nil) )
  }
  "creates ensureIndex query" in{
    import MongoImplicits._
    ensureIndex("index").on("collection", "address.city") mustEqual ( MongoEnsureIndexQuery("collection", "index", JPath("address.city") :: Nil, false) )
  }
  "creates dropIndex query" in{
    import MongoImplicits._
    dropIndex("index").on("collection") mustEqual ( MongoDropIndexQuery("collection", "index") )
  }
  "creates dropIndexes query" in{
    import MongoImplicits._
    dropIndexes.on("collection") mustEqual ( MongoDropIndexesQuery("collection") )
  }
  "creates ensureUniqueIndex query" in{
    import MongoImplicits._
    ensureUniqueIndex("index").on("collection", "address.city") mustEqual ( MongoEnsureIndexQuery("collection", "index", JPath("address.city") :: Nil, true) )
  }
  "creates update query" in{
    import MongoImplicits._
    update("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject) )
  }
  "creates updateMany query" in{
    import MongoImplicits._
    updateMany("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, false, true) )
  }
  "creates upsert query" in{
    import MongoImplicits._
    upsert("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, true, false) )
  }
  "creates upsertMany query" in{
    import MongoImplicits._
    upsertMany("collection").set(jObject) mustEqual ( MongoUpdateQuery("collection", jObject, None, true, true) )
  }
}
