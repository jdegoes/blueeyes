package blueeyes.persistence.mongo

import collection.immutable.ListSet

private[mongo] trait MongoFilters {
  def nearFilter(mongoFilter: MongoFilter): Option[MongoFieldFilter] = {
    def findNearFilter(filters: ListSet[MongoFilter]) = filters.find(nearFilter(_) != None).map(_.asInstanceOf[MongoFieldFilter])
    mongoFilter match {
      case e @ MongoFieldFilter(lhs, (MongoFilterOperators.$near | MongoFilterOperators.$nearSphere), _)  => Some(e)
      case MongoOrFilter(filters)  => findNearFilter(filters)
      case MongoAndFilter(filters) => findNearFilter(filters)
      case _ => None
    }
  }
}