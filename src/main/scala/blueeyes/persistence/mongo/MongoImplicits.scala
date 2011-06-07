package blueeyes.persistence.mongo

import blueeyes.json.JsonAST.JObject
import blueeyes.json.{JPathImplicits, JPath}

trait MongoImplicits extends JPathImplicits with MongoFilterImplicits with MongoQueryBuilder{
  implicit def stringToMongoCollection(string: String): MongoCollection = MongoCollectionReference(string)

  implicit def jpathToMongoSort(jpath: JPath): MongoSort = MongoSort(jpath, MongoSortOrderAscending)

  implicit def stringToMongoSort(string: String): MongoSort = MongoSort(JPath(string), MongoSortOrderAscending)

  implicit def stringToMongoUpdateBuilder(string: String): MongoUpdateBuilder = MongoUpdateBuilder(JPath(string))

  implicit def jpathToMongoUpdateBuilder(jpath: JPath): MongoUpdateBuilder = MongoUpdateBuilder(jpath)

  implicit def jvalueToMongoUpdateObject(value: JObject): MongoUpdateObject = MongoUpdateObject(value)
}

object MongoImplicits extends MongoImplicits

// vim: set ts=4 sw=4 et:
