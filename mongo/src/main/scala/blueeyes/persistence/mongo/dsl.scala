package blueeyes.persistence.mongo

import blueeyes.json._
import MongoFilterOperators._


object dsl extends MongoQueryBuilder {
  implicit def stringToMongoCollection(string: String): MongoCollection = MongoCollectionReference(string)

  implicit def jpathToMongoSort(jpath: JPath): MongoSort = MongoSort(jpath, MongoSortOrderAscending)

  implicit def stringToMongoSort(string: String): MongoSort = MongoSort(JPath(string), MongoSortOrderAscending)

  implicit def stringToMongoUpdateBuilder(string: String): MongoUpdateBuilder = MongoUpdateBuilder(JPath(string))

  implicit def jpathToMongoUpdateBuilder(jpath: JPath): MongoUpdateBuilder = MongoUpdateBuilder(jpath)

  implicit def jvalueToMongoUpdateObject(value: JObject): MongoUpdateObject = MongoUpdateObject(value)

  implicit def stringToHint(indexName: String) = NamedHint(indexName)

  implicit def keysToHint(keys: List[JPath]) = KeyedHint(keys)

  implicit def mongoOperatorToSymbolString(op: MongoFilterOperator): String = op.symbol

  implicit def stringToMongoFilterBuilder(string: String): MongoFilterBuilder = MongoFilterBuilder(JPath(string))

  implicit def jpathToMongoFilterBuilder(jpath: JPath): MongoFilterBuilder = MongoFilterBuilder(jpath)

  implicit def optionToMongoFilter(opt: Option[MongoFilter]): MongoFilter = opt.getOrElse(MongoFilterAll)

  implicit def jvalueToMongoPrimitive(value: JValue): MongoPrimitive = value match {
    case x: JString => MongoPrimitiveString(x.value)
    case x: JNum    => MongoPrimitiveDouble(x.toDouble)
    case x: JBool   => MongoPrimitiveBoolean(x.value)
    case x: JObject => MongoPrimitiveJObject(x)
    case x: JArray  => MongoPrimitiveArray(x.elements.map(jvalueToMongoPrimitive))
    case JNull | JUndefined => MongoPrimitiveNull
  }

  implicit def optionToMongoPrimitive[T <% MongoPrimitive](value: Option[T]) = MongoPrimitiveOption(value.map(a => a : MongoPrimitive))
  implicit def stringToMongoPrimitive(value: String)   = MongoPrimitiveString(value)
  implicit def longToMongoPrimitive(value: Long)       = MongoPrimitiveLong(value)
  implicit def intToMongoPrimitive(value: Int)         = MongoPrimitiveInt(value)
  implicit def bigIntToMongoPrimitive(value: BigInt)   = MongoPrimitiveBigInt(value)
  implicit def doubleToMongoPrimitive(value: Double)   = MongoPrimitiveDouble(value)
  implicit def booleanToMongoPrimitive(value: Boolean) = MongoPrimitiveBoolean(value)
  implicit def arrayToMongoPrimitive(value: List[MongoPrimitive]) = MongoPrimitiveArray(value)
}


// vim: set ts=4 sw=4 et:
