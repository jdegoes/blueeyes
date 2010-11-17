package blueeyes.persistence.mongo.mock

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
import blueeyes.js.RhinoJson._
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.JPathExtension._
import org.mozilla.javascript.Scriptable

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

  def requestDone = {}

  def requestStart = {}
}

private[mongo] class MockDatabaseCollection() extends DatabaseCollection with JObjectFieldsExtractor with MockIndex{
  private var container = JArray(Nil)

  def insert(objects: List[JObject]): Unit = {
    index(objects)
    insert0(objects)
  }

  def remove(filter: Option[MongoFilter]) : Unit = remove(search(filter))

  def count(filter: Option[MongoFilter]) = search(filter).size

  def insert0(objects: List[JObject]) = container = JArray(container.elements ++ objects)

  def indexed = all

  private def search(filter: Option[MongoFilter]): List[JObject] = filter.map(JObjectsFilter(all, _).map(_.asInstanceOf[JObject])).getOrElse(all)

  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])

  private def remove(objects: List[JObject]): Unit = container = JArray(all filterNot (objects contains))

  def distinct(selection: JPath, filter: Option[MongoFilter]) =
    search(filter).map(jobject => selectByPath(selection, jobject, (v) => {Some(v)}, (p, v) => {v})).filter(_.isDefined).map(_.get).distinct

  def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean){
    var objects = if (multi) search(filter) else search(filter).headOption.map(_ :: Nil).getOrElse(Nil)
    var updated = objects.map(update(_, value))

    if (objects.isEmpty && upsert){
      objects = value match{
        case MongoUpdateObject(value) => value :: Nil
        case _ => Nil
      }
      updated = objects
    }

    index(updated)
    remove(objects)
    insert0(updated)
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

    groupedObject.foreach(group => {
      val groupValue = group._2.foldLeft(initial){ (groupResult, current) => RhinoScript(reduceFunction.format(renderJObject(current), renderJObject(groupResult)))().get}
      result = group._1.merge(groupValue) :: result
    })

    JArray(result)
  }

  def renderJObject(value: JValue) = compact(render(value))

  private def groupObject(selection: MongoSelection, objects: List[JObject]) = {
    val groupedObjects = ValuesGroup[JValue, JObject]()

    def updateValue(value: JValue) = value match{
      case JNothing => Some(JNull)
      case _ => Some(value)
    }
    objects.foreach(jobject => {
      val fields      = selectFields(jobject :: Nil, selection.selection, updateValue _, (p, v) => {JObject(JField(toMongoField(p), v) :: Nil)}).head
      groupedObjects.emitCorrect(fields, jobject)
    })
    groupedObjects.group
  }

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = {
    val mapped  = mapObjects(map, filter)
    val reduced = reduceObjects(reduce, mapped)

    val collection = new MockDatabaseCollection()
    collection.insert(reduced)

    new MockMapReduceOutput(collection)
  }

  private def reduceObjects(reduce: String, mapped: Map[Any, List[JObject]]): List[JObject] = {
    val reduceScriptPattern = """var reduce = %s; reduce(%s, %s)"""
    val reduced = mapped.map(entry => {
      val entryKey = entry._1 match {
        case e: JValue => render(e)
        case e: String => "\"" + e + "\""
        case _ => entry._1
      }
      val entryValue = renderJObject(JArray(entry._2))
      val reducedObject = RhinoScript(reduceScriptPattern.format(reduce, entryKey, entryValue))().get
      Tuple2(entry._1, reducedObject)
    })
    reduced.values.toList
  }

  private def mapObjects(map: String, filter: Option[MongoFilter]) = {
    def keyTransformer(value: Any): Any = value match{
      case e: Scriptable => scriptableObject2JObject(e)
      case _ => value
    }
    def valueTransformer(value: Any): JObject = value match{
      case e: Scriptable => scriptableObject2JObject(e)
      case _ => error("value is not Json")
    }
    val mapScriptPattern = """var record  = %s; record.map  = %s; var emit = function(k, v){emitter.emit(k, v)}; record.map()"""
    val objects            = search(filter)
    val mapped             = ValuesGroup[Any, JObject](keyTransformer _, valueTransformer _)

    objects.foreach(jobject => RhinoScript(mapScriptPattern.format(renderJObject(jobject), map))(Map("emitter" -> mapped)))

    mapped.group
  }
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

private[mongo] class MockMapReduceOutput(output: MockDatabaseCollection) extends MapReduceOutput{
  def drop = {}

  def outpotCollection = MongoCollectionHolder(output)
}


private[mongo] case class ValuesGroup[K, V](keyTransformer : (Any) => K = (v: Any) => {error("any key is not supported")}, valueTransformer :(Any) => V = (v: Any) => {error("any value is not supported")}){
  private var groupedValues = Map[K, List[V]]()

  def emit(key: Any, value: Any) = {
    emitCorrect(keyTransformer(key), valueTransformer(value))
  }

  def emitCorrect(key: K, value: V){
    val grouped   = value :: groupedValues.get(key).getOrElse(Nil)
    groupedValues = groupedValues + Tuple2(key, grouped)
  }

  def group: Map[K, List[V]] = groupedValues
}