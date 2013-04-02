package blueeyes.core.http

/* For use in the If-Range Http Header */

/* If Range takes either an EntityTag -or- an HttpDateRange */
sealed trait IfRange  {

  def tag: Option[EntityTag]
  def date: Option[HttpDateTime]

  def value: String = {
    if (tag != None) 
      tag.get.toString
    else if (date != None)
      date.get.toString
    else  
      ""
  }

  override def toString = value

}


object IfRanges {

  def parseIfRanges(inString: String): Option[IfRange] = {
    /* First try to parse Entity Tags */
    EntityTags.parseEntityTags(inString).map(TagRange(_)) orElse 
    HttpDateTimes.parseHttpDateTimes(inString).map(DateRange(_))
  }


  case class TagRange(tag: Option[EntityTag]) extends IfRange {
    def date = None
  }

  object TagRange {
    def apply(tag: EntityTag): IfRange = TagRange(Some(tag))
  }


  case class DateRange(date: Option[HttpDateTime]) extends IfRange {
    def tag = None
  }

  object DateRange {
    def apply(date: HttpDateTime): IfRange = DateRange(Some(date))
  }


}
