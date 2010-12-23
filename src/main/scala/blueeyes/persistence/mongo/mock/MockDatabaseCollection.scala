package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.json.{JPath}
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.MongoFilterEvaluator._

private[mongo] class MockDatabaseCollection() extends DatabaseCollection with JObjectFieldsExtractor with MockIndex{
  private var container = JArray(Nil)

  def insert(objects: List[JObject]): Unit = {
    index(objects)
    insert0(objects)
  }

  def remove(filter: Option[MongoFilter]) : Unit = remove(search(filter))

  def count(filter: Option[MongoFilter]) = search(filter).size

  def indexed = all

  def distinct(selection: JPath, filter: Option[MongoFilter]) =
    search(filter).map(jobject => selectByPath(selection, jobject, (v) => {Some(v)}, (p, v) => {v})).filter(_.isDefined).map(_.get).distinct

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String) = GroupFunction(selection, initial, reduce, search(filter))

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = MapReduceFunction(map, reduce, outputCollection, search(filter))  

  private def insert0(objects: List[JObject]) = container = JArray(container.elements ++ objects)

  private def search(filter: Option[MongoFilter]): List[JObject] = filter.map(all.filter(_).map(_.asInstanceOf[JObject])).getOrElse(all)

  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])

  private def remove(objects: List[JObject]): Unit = container = JArray(all filterNot (objects contains))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean){
    var objects = if (multi) search(filter) else search(filter).headOption.map(_ :: Nil).getOrElse(Nil)
    var updated = UpdateFunction(value, objects)

    if (upsert){
      if (objects.isEmpty){
        objects = value match{
          case MongoUpdateObject(value) => value :: Nil
          case _ => Nil
        }
        updated = objects
      }
      else{
        remove(objects)
      }
    }

    index(updated)
    remove(objects)
    insert0(updated)
  }

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
    val objects = search(filter)
    val sorted  = sort.map(v => objects.sorted(new JObjectXPathBasedOrdering(v.sortField, v.sortOrder.order))).getOrElse(objects)
    val skipped = skip.map(sorted.drop(_)).getOrElse(sorted)
    val limited = limit.map(skipped.take(_)).getOrElse(skipped)

    selectExistingFields(limited, selection.selection).map(_.asInstanceOf[JObject]).toStream
  }
}

private[mongo] class MockMapReduceOutput(output: MockDatabaseCollection) extends MapReduceOutput{
  def drop = {}

  def outpotCollection = MongoCollectionHolder(output)
}
