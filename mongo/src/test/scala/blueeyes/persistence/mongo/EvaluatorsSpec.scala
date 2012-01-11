package blueeyes.persistence.mongo

import org.specs2.mutable.Specification

class EvaluatorsSpec extends Specification{
  private val palygon = Array((-2.0, 3.0), (-2.0, 1.0), (-1.0, 1.0), (-1.0, 3.0))
  "inPolygon" should{
    "does not contain outer point" in {
      Evaluators.inPolygon((2.0, 3.0), palygon) must be_==(false)
    }
    "contains inner point" in {
      Evaluators.inPolygon((-1.0, 2.0), palygon) must be_==(true)
    }
  }
}