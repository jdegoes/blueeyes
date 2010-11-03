package blueeyes.persistence.mongo

import MongoFilterOperators._
import blueeyes.json.JPath
import collection.immutable.List
import com.google.inject.{Provider, Inject, AbstractModule}
import blueeyes.persistence.mongo.MongoFilterOperators.MongoFilterOperator
import blueeyes.json.JsonAST._

class MockMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[MockMongoProvider])
  }
}

@com.google.inject.Singleton
class MockMongoProvider @Inject() () extends Provider[Mongo]{
  private val factory = new MockMongo()
  def get = factory
}

@com.google.inject.Singleton
class MockMongo() extends Mongo{
  private val databases     = scala.collection.mutable.Map[String, MockMongoDatabase]()
  def database(databaseName: String) = {
    databases.get(databaseName) match{
      case Some(x) => x
      case None =>{
        val mongoDatabase  = new MockMongoDatabase()
        databases.put(databaseName, mongoDatabase)
        mongoDatabase
      }
    }
  }
}

class MockMongoDatabase() extends MongoDatabase{
  private val collections   = scala.collection.mutable.Map[String, MockDatabaseCollection]()

  def apply[T](query: MongoQuery[T]): T     = query(collection(query.collection.name))
  def collection(collectionName: String) = {
    collections.get(collectionName) match{
      case Some(x) => x
      case None =>{
        val collection  = new MockDatabaseCollection()
        collections.put(collectionName, collection)
        collection
      }
    }
  }
}

class MockDatabaseCollection() extends DatabaseCollection{
  private var container = JArray(Nil)

  def insert(objects: List[JObject]) = {container = JArray(container.elements ++ objects)}

  def remove(filter: Option[MongoFilter]) = {
    val foundObjects: List[JObject] = filter.map(search(_)).getOrElse(all)
    foundObjects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))
    foundObjects.size
  }
  private def search(filter: MongoFilter): List[JObject] = JObjectsFilter(all, filter)

  def update(filter: Option[MongoFilter], value : JObject, upsert: Boolean, multi: Boolean) = {0}

  def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = {}

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
    filter.map(search(_)).getOrElse(all)
  }
  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])
}

trait MongoFieldEvaluator[T <: JValue] extends Function2[T, T, Boolean]

object JObjectsFilter{
  def apply(jobjects: List[JObject], filter: MongoFilter):  List[JObject] = filter match{
    case x: MongoFieldFilter => searchByField(jobjects, x)
    case x: MongoOrFilter    => x.queries.foldLeft(List[JObject]()){ (objects, filter0) => objects ++ JObjectsFilter(jobjects, filter0) } 
    case x: MongoAndFilter   => Nil
  }

  private def searchByField(jobjects: List[JObject], filter: MongoFieldFilter) = {
    val evaluator = FieldFilterEvalutors(filter.operator)
    jobjects.filter(jobject => {
      val value = jobject.get(filter.lhs)
      !value.filter(v => evaluator(v, filter.rhs.toJValue)).isEmpty
    })
  }
}

object FieldFilterEvalutors{
  def apply(operator: MongoFilterOperator): FieldFilterEvalutor = operator match{
    case $gt      => GtFieldFilterEvalutor
    case $gte     => GteFieldFilterEvalutor
    case $lt      => LtFieldFilterEvalutor
    case $lte     => LteFieldFilterEvalutor
    case $eq      => EqFieldFilterEvalutor
    case $ne      => NeFieldFilterEvalutor
    case $in      => InFieldFilterEvalutor
    case $nin     => NinFieldFilterEvalutor
    case $mod     => ModFieldFilterEvalutor
    case $all     => AllFieldFilterEvalutor
    case $size    => SizeFieldFilterEvalutor
    case $exists  => ExistsFieldFilterEvalutor
    case $type    => TypeFieldFilterEvalutor
    case $or      => error("'or' is not supported")
  }
}

sealed trait FieldFilterEvalutor extends Function2[JValue, JValue, Boolean]

case object EqFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = v1 == v2
}
case object NeFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = v1 != v2
}
case object GtFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = (v1, v2) match {
    case (JString(x1),  JString(x2)) => x1 > x2
    case (JInt(x1),     JInt(x2))    => x1 > x2
    case (JDouble(x1),  JDouble(x2)) => x1 > x2
    case (JDouble(x1),  JInt(x2))    => x1 > x2.doubleValue
    case (JInt(x1),     JDouble(x2)) => x1.doubleValue > x2
    case (JBool(x1),    JBool(x2))   => x1 > x2
    case _ => false
  }
}
case object GteFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = EqFieldFilterEvalutor(v1, v2) || GtFieldFilterEvalutor(v1, v2)
}
case object LtFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = (v1, v2) match {
    case (JString(x1),  JString(x2)) => x1 < x2
    case (JInt(x1),     JInt(x2))    => x1 < x2
    case (JDouble(x1),  JDouble(x2)) => x1 < x2
    case (JDouble(x1),  JInt(x2))    => x1 < x2.doubleValue
    case (JInt(x1),     JDouble(x2)) => x1.doubleValue < x2
    case (JBool(x1),    JBool(x2))   => x1 < x2
    case _ => false
  }
}
case object LteFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = EqFieldFilterEvalutor(v1, v2) || LtFieldFilterEvalutor(v1, v2)
}
case object InFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = v2 match {
    case JArray(x)  => x.exists(_ == v1)
    case JObject(x) => x.exists(_ == v1)
    case _ => false
  }
}
case object NinFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = v2 match {
    case JArray(x)  => !x.exists(_ == v1)
    case JObject(x) => !x.exists(_ == v1)
    case _ => false
  }
}
case object ModFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = (v1, v2) match {
    case (JInt(x1),     JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1 == y2
    case (JDouble(x1),  JArray(JInt(y1) :: JInt(y2) :: Nil))    => x1 % y1.toDouble == y2
    case _ => false
  }
}
case object AllFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = (v1, v2) match {
    case (JArray(x1), JArray(x2)) => (x1 filterNot(x2 contains) ).isEmpty 
    case _ => false
  }
}
case object SizeFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = (v1, v2) match {
    case (JArray(x1), JInt(x2)) => x1.size == x2
    case _ => false
  }
}
case object ExistsFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = true
}
case object TypeFieldFilterEvalutor extends FieldFilterEvalutor{
  def apply(v1: JValue, v2: JValue) = v2 match {
    case JInt(x) => (v1, x.intValue) match{
      case (JString(_), 2)  => true
      case (JDouble(_), 1)  => true
      case (JObject(_), 3)  => true
      case (JArray(_),  4)  => true
      case (JBool(_),   8)  => true
      case (JNull,      10) => true
      case (JInt(_),    18) => true
      case _ => false
    }
    case _ => false
  }
}