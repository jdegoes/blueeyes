package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.js.{RhinoScript}
import com.mongodb.MongoException
import blueeyes.persistence.mongo.JPathExtension._

object GroupFunction extends JObjectFieldsExtractor{
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