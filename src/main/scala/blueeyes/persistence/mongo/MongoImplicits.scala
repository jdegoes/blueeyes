package blueeyes.persistence.mongo

import blueeyes.json.JsonAST.JObject
import blueeyes.json.{JPathImplicits, JPath}
import collection.immutable.ListSet

trait MongoImplicits extends JPathImplicits with MongoFilterImplicits with MongoQueryBuilder{
  implicit def stringToMongoCollection(string: String): MongoCollection = MongoCollectionReference(string)

  implicit def jpathToMongoSort(jpath: JPath): MongoSort = MongoSort(jpath, MongoSortOrderAscending)

  implicit def stringToMongoSort(string: String): MongoSort = MongoSort(JPath(string), MongoSortOrderAscending)

  implicit def stringToMongoUpdateBuilder(string: String): MongoUpdateBuilder = MongoUpdateBuilder(JPath(string))

  implicit def jpathToMongoUpdateBuilder(jpath: JPath): MongoUpdateBuilder = MongoUpdateBuilder(jpath)

  implicit def jvalueToMongoUpdateObject(value: JObject): MongoUpdateObject = MongoUpdateObject(value)

  implicit def stringToHint(indexName: String) = NamedHint(indexName)

  implicit def keysToHint(keys: List[JPath]) = KeyedHint(ListSet(keys: _*))
}

object MongoImplicits extends MongoImplicits