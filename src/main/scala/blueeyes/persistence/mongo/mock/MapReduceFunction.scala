package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import org.mozilla.javascript.Scriptable
import blueeyes.js.{RhinoJsonImplicits, RhinoScript}

private[mongo] object MapReduceFunction extends RhinoJsonImplicits{
  def apply(map: String, reduce: String, outputCollection: Option[String], objects: List[JObject]) = {
    val mapped  = mapObjects(map, objects)
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
      case _ => error("value is not Json")
    }
    val mapScriptPattern = """var record  = %s; record.map  = %s; var emit = function(k, v){emitter.emit(k, v)}; record.map()"""
    val mapped          = ValuesGroup[Any, JObject](keyTransformer _, valueTransformer _)

    objects.foreach(jobject => RhinoScript(mapScriptPattern.format(renderJObject(jobject), map))(Map("emitter" -> mapped)))

    mapped.group
  }

  private def renderJObject(value: JValue) = compact(render(value))
}