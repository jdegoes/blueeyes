package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath
import blueeyes.json.JsonAST._

class MongoQueryBuilderSpec  extends Specification{
  private val jObject = JObject(JField("Foo", JString("bar")) :: Nil)

  "creates select query" in{
    select("foo", "bar").from("collection") mustEqual ( MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection")) )
  }
  "creates selectOne query" in{
    selectOne("foo", "bar").from("collection") mustEqual ( MongoSelectOneQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection")) )
  }
  "creates remove query" in{
    remove.from("collection") mustEqual ( MongoRemoveQuery(MongoCollection("collection")) )
  }
  "creates insert query" in{
    insert(jObject).into("collection") mustEqual ( MongoInsertQuery(MongoCollection("collection"), jObject :: Nil) )
  }
  "creates update query" in{
    update("collection").set(jObject) mustEqual ( MongoUpdateQuery(MongoCollection("collection"), jObject) )
  }
  "creates updateMany query" in{
    updateMany("collection").set(jObject) mustEqual ( MongoUpdateQuery(MongoCollection("collection"), jObject, None, false, true) )
  }
  "creates upsert query" in{
    upsert("collection").set(jObject) mustEqual ( MongoUpdateQuery(MongoCollection("collection"), jObject, None, true, false) )
  }
  "creates upsertMany query" in{
    upsertMany("collection").set(jObject) mustEqual ( MongoUpdateQuery(MongoCollection("collection"), jObject, None, true, true) )
  }
}