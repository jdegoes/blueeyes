package blueeyes.persistence.mongo

import org.spex.Specification
import MongoFilterOperators._

class MongoFilterOperatorSpec extends Specification{
  "$gt opposite operator is $lte" in {
    $gt.unary_! must be ($lte)
  }
  "$gte opposite operator is $lt" in {
    $gte.unary_! must be ($lt)
  }
  "$lt opposite operator is $gte" in {
    $lt.unary_! must be ($gte)
  }
  "$lte opposite operator is $gt" in {
    $lte.unary_! must be ($gt)
  }
  "$eq opposite operator is $ne" in {
    $eq.unary_! must be ($ne)
  }
  "$in opposite operator is $nin" in {
    $in.unary_! must be ($nin)
  }
  "$mod does not have a negation" in {
    $mod.unary_! must throwAn[java.lang.RuntimeException]
  }
  "$all does not have a negation" in {
    $all.unary_! must throwAn[java.lang.RuntimeException]
  }
  "$size does not have a negation" in {
    $size.unary_! must throwAn[java.lang.RuntimeException]
  }
  "$exists does not have a negation" in {
    $exists.unary_! must throwAn[java.lang.RuntimeException]
  }
  "$type does not have a negation" in {
    $type.unary_! must throwAn[java.lang.RuntimeException]
  }
  "$or does not have a negation" in {
    $or.unary_! must throwAn[java.lang.RuntimeException]
  }
}