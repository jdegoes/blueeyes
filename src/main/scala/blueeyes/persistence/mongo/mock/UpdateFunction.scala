package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import blueeyes.json.{JPath}
import MockMongoUpdateEvaluators._

private[mock] object UpdateFunction extends JObjectFieldsExtractor{

  def apply(value : MongoUpdate, objects: List[JObject]) = objects.map(update(_, value))

  private def update(jobject: JObject, value : MongoUpdate): JObject = value match {
    case MongoUpdateNothing         => jobject
    case x: MongoUpdateObject       => x.value
    case x: MongoUpdateField   => {
      def updateValue(value: JValue) = Some(UpdateFiledEvaluatorFactory(x.operator)(value, x.filter))
      val (mergeOperation, pathRestorer) = jobject.get(x.path) match {
        case List(JNothing) => (true, jvalueToJObject _)
        case _   => (false, (p: JPath, v: JValue) => {v})
      }

      val jfield = selectByPath(x.path, jobject, updateValue _, pathRestorer)
      jfield.map(newValue => (if (mergeOperation) jobject.merge(newValue) else jobject.replace(x.path, v => {newValue})).asInstanceOf[JObject]).getOrElse(jobject)
    }
    case x: MongoUpdateFields => x.list.foldLeft(jobject){(jobject, updater) => update(jobject, updater)}
  }
}