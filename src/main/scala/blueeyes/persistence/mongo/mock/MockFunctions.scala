package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JPath
import blueeyes.js.{RhinoJsonImplicits, RhinoScript}
import com.mongodb.MongoException
import blueeyes.persistence.mongo.JPathExtension._
import org.mozilla.javascript.Scriptable

object GroupFunction extends JObjectFields{
  def apply(selection: MongoSelection, initial: JObject, reduce: String, objects: List[JObject]) = {
    val FunctionPattern = "[ ]*function[ ]*\\((.+),(.+)\\)[ ]*\\{(.+)\\}".r

    val (param1, param2, fun) = reduce match{
      case FunctionPattern(val1, val2, val3) => (val1, val2, val3)
      case _ => throw new MongoException("reduce function is broken")
    }

    val reduceFunction = "f = function(%s, %s){%s ; return %s;};".format(param1, param2, fun, param2) + "f(%s, %s)"
    val groupedObject  = groupObject(selection, objects)
    var result         = List[JValue]()

    groupedObject.foreach(group => {
      val groupValue = group._2.foldLeft(initial){ (groupResult, current) => RhinoScript(reduceFunction.format(renderJObject(current), renderJObject(groupResult)))().get}
      result = group._1.merge(groupValue) :: result
    })

    JArray(result)
  }

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

  private def renderJObject(value: JValue) = compact(render(value))
}

private[mongo] object MapReduceFunction extends RhinoJsonImplicits{
  def apply(map: String, reduce: String, outputCollection: Option[String], objects: List[JObject], database: MockDatabase) = {
    val mapped  = mapObjects(map, objects)
    val reduced = reduceObjects(reduce, mapped)

    val collection = new MockDatabaseCollection("reduced", database)
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
      val entryValue    = renderJObject(JArray(entry._2))
      val reducedObject = RhinoScript(reduceScriptPattern.format(reduce, entryKey, entryValue))().get
      Tuple2(entry._1, reducedObject)
    })
    reduced.values.toList
  }

  private def mapObjects(map: String, objects: List[JObject]) = {
    def keyTransformer(value: Any): Any = value match{
      case e: Scriptable => scriptableObject2JObject(e)
      case _ => value
    }
    def valueTransformer(value: Any): JObject = value match{
      case e: Scriptable => scriptableObject2JObject(e)
      case _ => sys.error("value is not Json")
    }
    val mapScriptPattern = """var record  = %s; record.map  = %s; var emit = function(k, v){emitter.emit(k, v)}; record.map()"""
    val mapped          = ValuesGroup[Any, JObject](keyTransformer _, valueTransformer _)

    objects.foreach(jobject => RhinoScript(mapScriptPattern.format(renderJObject(jobject), map))(Map("emitter" -> mapped)))

    mapped.group
  }

  private def renderJObject(value: JValue) = compact(render(value))
}

private[mock] object UpdateFunction extends JObjectFields{

  def apply(value : MongoUpdate, objects: List[JObject]) = objects.map(update(_, value))

  private def update(jobject: JObject, value : MongoUpdate): JObject = value match {
    case MongoUpdateNothing         => jobject
    case x: MongoUpdateObject       => x.value
    case x: MongoUpdateField   => {
      def updateValue(value: JValue) = Some(MockMongoUpdateEvaluators.UpdateFiledEvaluatorFactory(x.operator)(value, x.filter))
      val (mergeOperation, pathRestorer) = jobject.get(x.path) match {
        case JNothing => (true, jvalueToJObject _)
        case _   => (false, (p: JPath, v: JValue) => {v})
      }

      val jfield = selectByPath(x.path, jobject, updateValue _, pathRestorer)
      jfield.map(update(jobject, x.path, _, mergeOperation)).getOrElse(jobject)
    }
    case x: MongoUpdateFields => x.list.foldLeft(jobject){(jobject, updater) => update(jobject, updater)}
  }

  private def update(jobject: JObject, fieldPath: JPath, newValue: JValue, merge: Boolean): JObject = {
    val updated = if (merge) jobject.merge(newValue) else jobject.replace(fieldPath, v => {newValue})
    updated.remove(_ == JNothing).asInstanceOf[JObject]
  }
}

private[mongo] case class ValuesGroup[K, V](keyTransformer : (Any) => K = (v: Any) => {sys.error("any key is not supported")}, valueTransformer :(Any) => V = (v: Any) => {sys.error("any value is not supported")}){
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