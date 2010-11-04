package blueeyes.persistence.mongo

import blueeyes.json.JPath
import collection.immutable.List
import com.google.inject.{Provider, Inject}
import blueeyes.json.JsonAST._
import JPathExtension._
import MockMongoFiltersEvalutors._
import MockMongoUpdateEvalutors._
import com.mongodb.MongoException

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
      insert0(objects)
    }

    def insert0(objects: List[JObject]) = container = JArray(container.elements ++ objects)

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
      val objects = search(filter)
      objects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))
      objects.size
    }
    def remove0(objects: List[JObject]) = objects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))

    def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean): Int = {
      val objects = if (multi) search(filter) else search(filter).headOption.map(_ :: Nil).getOrElse(Nil)
      val updated = objects.map(update(_, value))

      checkIndex(updated)
      remove0(objects)
      insert0(updated)

      objects.size
    }

    private def update(jobject: JObject, value : MongoUpdateValue): JObject = value match{
      case x: MongoUpdateObject       => x.value
      case x: MongoUpdateFieldValue   => {
        def updateValue(value: JValue) = UpdateFiledEvalutorFactory(x.operator)(value, x.value)
        val jfield = selectByPath(x.lhs, jobject, updateValue _)
        jfield.map(jobject.merge(_).asInstanceOf[JObject]).getOrElse(jobject)
      }
      case x: MongoUpdateFieldsValues => x.values.foldLeft(jobject){(jobject, updater) => update(jobject, updater)}
    }

    def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = {
      indexes = if (unique) indexes.get(name) match{
        case None    => indexes + Tuple2(name, keys)
        case Some(x) => indexes
      } else indexes
    }

    def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
      val objects = search(filter)
      val sorted  = sort.map(v => objects.sorted(new JObjectXPathBasedOrdering(v.sortField, v.sortOrder.order))).getOrElse(objects)
      val skipped = skip.map(sorted.drop(_)).getOrElse(sorted)
      val limited = limit.map(skipped.take(_)).getOrElse(skipped)

      selectFields(limited, selection)
    }
    private def selectFields(jobjects: List[JObject], selection : MongoSelection) = {
      if (!selection.selection.isEmpty) {
        val allJFields = jobjects.map(jobject => selection.selection.map(selectByPath(_, jobject, (jobject) => {jobject})))
        allJFields.map(jfields => {
          val definedJFields = jfields.filter(_ != None).map(_.get)
          definedJFields.headOption.map(head => definedJFields.tail.foldLeft(head){(jobject, jfield) => jobject.merge(jfield).asInstanceOf[JObject]})
        }).filter(_ != None).map(_.get)
      } else jobjects
    }
    private def selectByPath(selectionPath: JPath, jobject: JObject, transformer: (JValue) => JValue) = jobject.get(selectionPath) match{
      case JNothing :: Nil => None
      case Nil             => None
      case x :: Nil        => Some(jvalueToJObject(selectionPath, transformer(x)))
      case _        => error("jpath which is select more then one value is not supported")
    }

    private def jvalueToJObject(path: JPath, value: JValue) = {
      val elements = toMongoField(path).split("\\.").reverse
      elements.tail.foldLeft(JObject(JField(elements.head, value) :: Nil)){(result, element) => JObject(JField(element, result) :: Nil)}
    }

    private def search(filter: Option[MongoFilter]): List[JObject] = filter.map(JObjectsFilter(all, _)).getOrElse(all)

    private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])
  }

  object JObjectsFilter{
    def apply(jobjects: List[JObject], filter: MongoFilter):  List[JObject] = filter match{
      case x: MongoFieldFilter => searchByField(jobjects, x)
      case x: MongoOrFilter    => x.queries.foldLeft(List[JObject]()){ (objects, filter0) => objects.union(JObjectsFilter(jobjects, filter0)) }
      case x: MongoAndFilter   => x.queries.foldLeft(List[JObject]()){ (objects, filter0) => objects.intersect(JObjectsFilter(jobjects, filter0)) }
    }

    private def searchByField(jobjects: List[JObject], filter: MongoFieldFilter) = {
      val evaluator = FieldFilterEvalutorFactory(filter.operator)
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
}