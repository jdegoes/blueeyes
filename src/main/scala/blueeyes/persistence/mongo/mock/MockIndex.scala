package blueeyes.persistence.mongo.mock

import blueeyes.json.JPath
import blueeyes.json.JsonAST.JObject
import blueeyes.persistence.mongo.MongoSelection
import com.mongodb.MongoException
import scala.collection.immutable.ListSet

private[mock] trait MockIndex extends JObjectFields{
  private var indexes   = Map[String, Set[JPath]]()

  def ensureIndex(name: String, keys: ListSet[JPath], unique: Boolean) = {
    indexes = if (unique) indexes.get(name) match{
      case None    => indexes + Tuple2(name, keys)
      case Some(x) => indexes
    } else indexes
  }

  def dropIndex(name: String) = {
    indexes = indexes - name
  }

  def indexExists(name: String): Boolean = indexes.contains(name)
  def indexExists(keys: Set[JPath]): Boolean = indexes.find(keyAndValue => keys.toSet == keyAndValue._2.toSet) != None

  def dropIndexes = indexes = Map[String, Set[JPath]]()

  def index(newObjects: List[JObject]) = {
    indexes.foreach(index => {
      val selection = MongoSelection(index._2)
      val newFields = selectExistingFields(newObjects, selection.selection)

      if (newFields.distinct.size != newFields.size) throw new MongoException("Index contraint.")

      newObjects.foreach(jObject => {
        val existing  = selectExistingFields(indexed, selection.selection)
        if ((existing filterNot (newFields contains)).size != existing.size) throw new MongoException("Index contraint.")
      })
    })
  }

  def indexed: List[JObject]
}
