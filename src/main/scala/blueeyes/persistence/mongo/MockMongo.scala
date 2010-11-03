package blueeyes.persistence.mongo

import MongoFilterOperators._
import blueeyes.json.JPath
import collection.immutable.List
import com.google.inject.{Provider, Inject, AbstractModule}
import blueeyes.persistence.mongo.MongoFilterOperators.MongoFilterOperator
import blueeyes.json.JsonAST._
import JPathExtension._
import com.mongodb.MongoException

class MockMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[MockMongoImplementation.MockMongoProvider])
  }
}

private[mongo] object MockMongoImplementation{
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
    private var indexes   = Map[String, List[JPath]]()

    def insert(objects: List[JObject]) = {
      checkIndex(objects)
      container = JArray(container.elements ++ objects)
    }

    private def checkIndex(objects: List[JObject]) = {
      indexes.foreach(index => {
        val selection = MongoSelection(index._2)
        val newFields = selectFields(objects, selection)
        if (newFields.distinct.size != newFields.size) throw new MongoException("Index contraint.")
        objects.foreach(jobject => {
          val existing  = selectFields(all, selection)
          if ((existing filterNot (newFields contains)).size != existing.size) throw new MongoException("Index contraint.")
        })
      })
    }

    def remove(filter: Option[MongoFilter]) = {
      val foundObjects: List[JObject] = filter.map(search(all, _)).getOrElse(all)
      foundObjects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))
      foundObjects.size
    }

    def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean) = {

    0}

    def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = {
      indexes = if (unique) indexes.get(name) match{
        case None    => indexes + Tuple2(name, keys)
        case Some(x) => indexes
      } else indexes
    }

    def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
      val objects = filter.map(search(all, _)).getOrElse(all)
      val sorted  = sort.map(v => objects.sorted(new JObjectXPathBasedOrdering(v.sortField, v.sortOrder.order))).getOrElse(objects)
      val skipped = skip.map(sorted.drop(_)).getOrElse(sorted)
      val limited = limit.map(skipped.take(_)).getOrElse(skipped)

      selectFields(limited, selection)
    }
    private def selectFields(jobjects: List[JObject], selection : MongoSelection) = {
      if (!selection.selection.isEmpty) {
        val allJFields = jobjects.map(jobject => selection.selection.map(selectByPath(jobject, _)))
        allJFields.map(jfields => {
          val definedJFields = jfields.filter(_ != None).map(_.get)
          definedJFields.headOption.map(head => definedJFields.tail.foldLeft(head){(jobject, jfield) => jobject.merge(jfield).asInstanceOf[JObject]})
        }).filter(_ != None).map(_.get)
      } else jobjects
    }
    private def selectByPath(jobject: JObject, selectionPath: JPath) = jobject.get(selectionPath) match{
      case JNothing :: Nil => None
      case Nil             => None
      case x :: Nil        => {
        val elements = toMongoField(selectionPath).split("\\.").reverse
        Some(elements.tail.foldLeft(JObject(JField(elements.head, x) :: Nil)){(result, element) => JObject(JField(element, result) :: Nil)})
      }
      case _        => error("jpath which is select more then one value is not supported")
    }

    private def search(objects: List[JObject], filter: MongoFilter): List[JObject] = JObjectsFilter(objects, filter)

    private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])
  }

  trait MongoFieldEvaluator[T <: JValue] extends Function2[T, T, Boolean]

  object JObjectsFilter{
    def apply(jobjects: List[JObject], filter: MongoFilter):  List[JObject] = filter match{
      case x: MongoFieldFilter => searchByField(jobjects, x)
      case x: MongoOrFilter    => x.queries.foldLeft(List[JObject]()){ (objects, filter0) => objects.union(JObjectsFilter(jobjects, filter0)) }
      case x: MongoAndFilter   => x.queries.foldLeft(List[JObject]()){ (objects, filter0) => objects.intersect(JObjectsFilter(jobjects, filter0)) }
    }

    private def searchByField(jobjects: List[JObject], filter: MongoFieldFilter) = {
      val evaluator = FieldFilterEvalutors(filter.operator)
      jobjects.filter(jobject => {
        val value = jobject.get(filter.lhs)
        !value.filter(v => evaluator(v, filter.rhs.toJValue)).isEmpty
      })
    }
  }

  class JObjectXPathBasedOrdering(path: JPath, weight: Int) extends Ordering[JObject]{
    def compare(o1: JObject, o2: JObject) = (o1.get(path), o2.get(path)) match {
      case (v1 :: Nil, v2 :: Nil) =>
        (v1, v2) match {
          case (JString(x1),  JString(x2)) => x1.compare(x2) * weight
          case (JInt(x1),     JInt(x2))    => x1.compare(x2) * weight
          case (JDouble(x1),  JDouble(x2)) => x1.compare(x2) * weight
          case (JDouble(x1),  JInt(x2))    => x1.compare(x2.doubleValue) * weight
          case (JInt(x1),     JDouble(x2)) => x1.doubleValue.compare(x2) * weight
          case (JBool(x1),    JBool(x2))   => x1.compare(x2) * weight
          case (JNull,        JNull)       => 0
          case (v,            JNull)       => 1
          case (JNull,        v)           => -1
          case (JNothing,     JNothing)    => 0
          case (v,            JNothing)       => 1
          case (JNothing,     v)           => -1
          case _ => error("differents elements cannot be ordered")
        }
      case _ => error("lists cannot be ordered")
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
}