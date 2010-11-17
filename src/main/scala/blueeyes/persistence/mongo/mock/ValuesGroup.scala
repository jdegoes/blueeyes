package blueeyes.persistence.mongo.mock

private[mongo] case class ValuesGroup[K, V](keyTransformer : (Any) => K = (v: Any) => {error("any key is not supported")}, valueTransformer :(Any) => V = (v: Any) => {error("any value is not supported")}){
  private var groupedValues = Map[K, List[V]]()

  def emit(key: Any, value: Any) = {
    emitCorrect(keyTransformer(key), valueTransformer(value))
  }

  def emitCorrect(key: K, value: V){
    val grouped   = value :: groupedValues.get(key).getOrElse(Nil)
    groupedValues = groupedValues + Tuple2(key, grouped)
  }

  def group: Map[K, List[V]] = groupedValues
}