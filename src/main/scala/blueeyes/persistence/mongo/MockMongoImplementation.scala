package blueeyes.persistence.mongo

import collection.immutable.List
import blueeyes.json.JsonAST._
import MockMongoFiltersImplementation._
import MockMongoUpdateEvalutors._
import java.lang.String
import blueeyes.js.RhinoScript
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.Printer._
import blueeyes.json.{JPath}
import com.mongodb.{MongoException}
import JPathExtension._

private[mongo] class MockMongoDatabase() extends MongoDatabase{
  private val collections   = scala.collection.mutable.Map[String, MockDatabaseCollection]()

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

private[mongo] class MockDatabaseCollection() extends DatabaseCollection with JObjectFieldsExtractor{
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
      val newFields = selectExistingFields(objects, selection.selection)
      if (newFields.distinct.size != newFields.size) throw new MongoException("Index contraint.")
      objects.foreach(jobject => {
        val existing  = selectExistingFields(all, selection.selection)
        if ((existing filterNot (newFields contains)).size != existing.size) throw new MongoException("Index contraint.")
      })
    })
  }

  def remove(filter: Option[MongoFilter]) = {
    val objects = search(filter)
    objects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))
    objects.size
  }

  def count(filter: Option[MongoFilter]) = search(filter).size

  def remove0(objects: List[JObject]) = objects.foreach(jobject => container = JArray(container.elements.filterNot(_ == jobject)))

  def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean): Int = {
    var objects = if (multi) search(filter) else search(filter).headOption.map(_ :: Nil).getOrElse(Nil)
    var updated = objects.map(update(_, value))

    if (objects.isEmpty && upsert){
      objects = value match{
        case MongoUpdateObject(value) => value :: Nil
        case _ => Nil
      }
      updated = objects
    }

    checkIndex(updated)
    remove0(objects)
    insert0(updated)

    objects.size
  }

  private def update(jobject: JObject, value : MongoUpdateValue): JObject = value match {
    case x: MongoUpdateObject       => x.value
    case x: MongoUpdateFieldValue   => {
      def updateValue(value: JValue) = Some(UpdateFiledEvalutorFactory(x.operator)(value, x.filter))
      val (mergeOperation, pathRestorer) = jobject.get(x.path) match {
        case List(JNothing) => (true, jvalueToJObject _)
        case _   => (false, (p: JPath, v: JValue) => {v})
      }

      val jfield = selectByPath(x.path, jobject, updateValue _, pathRestorer)
      jfield.map(newValue => (if (mergeOperation) jobject.merge(newValue) else jobject.replace(x.path, v => {newValue})).asInstanceOf[JObject]).getOrElse(jobject)
    }
    case x: MongoUpdateFieldsValues => x.values.foldLeft(jobject){(jobject, updater) => update(jobject, updater)}
  }

  private def toPath(filter: MongoFilter) = filter match {
    case x: MongoFieldFilter => x.lhs
    case x: MongoElementsMatchFilter => x.lhs
    case _ => error("filter must be either MongoFieldFilter or MongoElementsMatchFilter")
  }

  def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = {
    indexes = if (unique) indexes.get(name) match{
      case None    => indexes + Tuple2(name, keys)
      case Some(x) => indexes
    } else indexes
  }


  def dropIndex(name: String) = {
    indexes = indexes - name
  }

  def dropIndexes = indexes = Map[String, List[JPath]]()

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
    val objects = search(filter)
    val sorted  = sort.map(v => objects.sorted(new JObjectXPathBasedOrdering(v.sortField, v.sortOrder.order))).getOrElse(objects)
    val skipped = skip.map(sorted.drop(_)).getOrElse(sorted)
    val limited = limit.map(skipped.take(_)).getOrElse(skipped)

    selectExistingFields(limited, selection.selection).map(_.asInstanceOf[JObject]).toStream
  }

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String) = {
    val FunctionPattern = "[ ]*function[ ]*\\((.+),(.+)\\)[ ]*\\{(.+)\\}".r

    val (param1, param2, fun) = reduce match{
      case FunctionPattern(val1, val2, val3) => (val1, val2, val3)
      case _ => throw new MongoException("reduce function is broken")
    }

    val reduceFunction = "f = function(%s, %s){%s ; return %s;};".format(param1, param2, fun, param2) + "f(%s, %s)"
    val groupedObject  = groupObject(selection, search(filter))
    var result         = List[JValue]()

    def renderJObject(value: JValue) = compact(render(value))
    groupedObject.foreach(group => {
      val groupValue = group._2.foldLeft(initial){ (groupResult, current) => RhinoScript(reduceFunction.format(renderJObject(current), renderJObject(groupResult)))()}
      result = group._1.merge(groupValue) :: result
    })

    JArray(result)
  }

  private def groupObject(selection: MongoSelection, objects: List[JObject]) = {
    var groupedObjects = Map[JValue, List[JObject]]()

    def updateValue(value: JValue) = value match{
      case JNothing => Some(JNull)
      case _ => Some(value)
    }
    objects.foreach(jobject => {
      val fields      = selectFields(jobject :: Nil, selection.selection, updateValue _, (p, v) => {JObject(JField(toMongoField(p), v) :: Nil)}).head
      val grouped     = jobject :: groupedObjects.get(fields).getOrElse(Nil)
      groupedObjects  = groupedObjects + Tuple2(fields, grouped)
    })
    groupedObjects
  }

  def distinct(selection: JPath, filter: Option[MongoFilter]) = {
    val objects = search(filter)
    objects.map(jobject => selectByPath(selection, jobject, (v) => {Some(v)}, (p, v) => {v})).filter(_.isDefined).map(_.get).distinct
  }


  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = {
    new MockMapReduceOutput(this)  
  }

  private def search(filter: Option[MongoFilter]): List[JObject] = filter.map(JObjectsFilter(all, _).map(_.asInstanceOf[JObject])).getOrElse(all)

  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])
}

private[mongo] class JObjectXPathBasedOrdering(path: JPath, weight: Int) extends Ordering[JObject]{
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

class MockMapReduceOutput(output: MockDatabaseCollection) extends MapReduceOutput{
  def drop = {}

  def outpotCollection = MongoCollectionHolder(output)
}
