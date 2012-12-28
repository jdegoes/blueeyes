package blueeyes.persistence.mongo

import blueeyes.json._
import blueeyes.util.ProductPrefixUnmangler
import MongoFilterOperators._

import scalaz._

object MongoFilterOperators {
  sealed trait MongoFilterOperator extends Product with ProductPrefixUnmangler {
    def symbol: String = unmangledName

    def unary_! : MongoFilterOperator

    override def toString = symbol
  }

  sealed trait MongoFilterOperatorBound extends MongoFilterOperator
  case object $gt   extends MongoFilterOperatorBound  { def unary_! = $lte; }
  case object $gte  extends MongoFilterOperatorBound  { def unary_! = $lt; }
  case object $lt   extends MongoFilterOperatorBound  { def unary_! = $gte; }
  case object $lte  extends MongoFilterOperatorBound  { def unary_! = $gt; }

  sealed trait MongoFilterOperatorEquality extends MongoFilterOperator
  case object $eq extends MongoFilterOperatorEquality { def unary_! = $ne; } // This is a virtual operator, it's not real!!!!
  case object $ne extends MongoFilterOperatorEquality { def unary_! = $eq; }
  case object $regex extends MongoFilterOperatorEquality { def unary_! : MongoFilterOperator  = sys.error("The $regex operator does not have a negation"); }

  sealed trait MongoFilterOperatorContainment extends MongoFilterOperator
  case object $in    extends MongoFilterOperatorContainment { def unary_! = $nin; }
  case object $nin   extends MongoFilterOperatorContainment { def unary_! = $in; }

  case object $mod         extends MongoFilterOperator { def unary_! : MongoFilterOperator = sys.error("The $mod operator does not have a negation"); }
  case object $all         extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $all operator does not have a negation"); }
  case object $size        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $size operator does not have a negation"); }
  case object $exists      extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $exists operator does not have a negation"); }
  case object $type        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $type operator does not have a negation"); }
  case object $or          extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $or operator does not have a negation"); }
  case object $and         extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $and operator does not have a negation"); }
  case object $each        extends MongoFilterOperator { def unary_! : MongoFilterOperator  = sys.error("The $each operator does not have a negation"); }

  sealed trait MongoFilterOperatorGeo extends MongoFilterOperator
  case object $near        extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $near operator does not have a negation"); }
  case object $nearSphere  extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $nearSphere operator does not have a negation"); }
  case object $within      extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $within operator does not have a negation"); }
  case object $where       extends MongoFilterOperatorGeo { def unary_! : MongoFilterOperator  = sys.error("The $where operator does not have a negation"); }
}

// vim: set ts=4 sw=4 et:
