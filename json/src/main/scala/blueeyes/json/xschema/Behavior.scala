package blueeyes.json.xschema {

import blueeyes.json.JsonAST._

object PredefinedProperties {
  val XSchemaDoc = "xschema.doc"
  val XSchemaIncludeSchemas = "xschema.includeSchemas"
}

trait XProductBehavior {
  def terms: List[XField]
  
  def realFields: List[XRealField] = terms.filter(_.isInstanceOf[XRealField]).map(_.asInstanceOf[XRealField])
  
  def viewFields: List[XViewField] = terms.filter(_.isInstanceOf[XViewField]).map(_.asInstanceOf[XViewField])
  
  def constantFields: List[XConstantField] = terms.filter(_.isInstanceOf[XConstantField]).map(_.asInstanceOf[XConstantField])
  
  def isSingleton: Boolean = (realFields.length == 0)
}

object XSchemaTree {
  trait XSchemaWalker[T] {
    def begin(data: T, defn:  XProduct): T = data
    def begin(data: T, defn:  XCoproduct): T = data
    def begin(data: T, defn:  XUnion): T = data
    def begin(data: T, field: XField): T = data
    def begin(data: T, opt:   XOptional): T = data
    def begin(data: T, col:   XCollection): T = data
    def begin(data: T, map:   XMap): T = data
    def begin(data: T, tuple: XTuple): T = data

    def separator(data: T): T = data

    def walk(data: T, const:  XConstant): T = data
    def walk(data: T, prim:   XPrimitiveRef): T = data
    def walk(data: T, prim:   XDefinitionRef): T = data

    def end(data: T, defn:  XProduct): T = data
    def end(data: T, defn:  XCoproduct): T = data
    def end(data: T, defn:  XUnion): T = data
    def end(data: T, field: XField): T = data
    def end(data: T, opt:   XOptional): T = data
    def end(data: T, col:   XCollection): T = data
    def end(data: T, map:   XMap): T = data
    def end(data: T, tuple: XTuple): T = data
  }
  
  def walk[T](s: XSchema, initial: T, walker: XSchemaWalker[T]): T = {
    def walkContainer(elements: Iterable[XSchema], initial: T): T = {
      var isFirst = true
    
      elements.foldLeft[T](initial) { (cur, x) => 
        if (isFirst) {
          isFirst = false; walk(x, cur, walker)
        }
        else {
          walk(x, walker.separator(cur), walker)
        }
      }
    }

    s match {
      case x: XProduct     => walker.end(walkContainer(x.terms, walker.begin(initial, x)), x)
      case x: XCoproduct   => walker.end(walkContainer(x.terms, walker.begin(initial, x)), x)
      case x: XUnion       => walker.end(walkContainer(x.terms, walker.begin(initial, x)), x)
      case x: XField       => walker.end(walkContainer(x.fieldType    :: Nil, walker.begin(initial, x)), x)
      case x: XOptional    => walker.end(walkContainer(x.optionalType :: Nil, walker.begin(initial, x)), x)
      case x: XCollection  => walker.end(walkContainer(x.elementType  :: Nil, walker.begin(initial, x)), x)
      case x: XMap         => walker.end(walkContainer(x.keyType :: x.valueType :: Nil, walker.begin(initial, x)), x)
      case x: XTuple       => walker.end(walkContainer(x.types, walker.begin(initial, x)), x)
      
      case x: XConstant      => walker.walk(initial, x)
      case x: XDefinitionRef => walker.walk(initial, x)
      case x: XPrimitiveRef  => walker.walk(initial, x)
    }
  }
}

}