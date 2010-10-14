package blueeyes.json.xschema.codegen {

import blueeyes.json.xschema._
import blueeyes.json.xschema.XSchemaTree._

case class XSchemaValidation(warnings: List[String], errors: List[String])

/** A database of schema definitions.
 */
trait XSchemaDatabase extends Iterable[XSchema] {
  def definitions: List[XDefinition]
  
  def constants: List[XConstant]
  
  /** Validates the definitions in the database, returning errors and warnings.
   */
  def validate: XSchemaValidation = {
    val errors = Nil
    val warnings = Nil
    
    XSchemaValidation(warnings, errors)
  }
  
  /** Finds all the terms of the specified multitype, regardless of how deep 
   * they are in the type hierarchy.
   */
  def findProductTerms(x: XMultitype): List[XProduct] = (resolve(elementsOf(x)).flatMap {
    case x: XProduct   => x :: Nil
    case x: XMultitype => findProductTerms(x)
    case _ => Nil
  }).distinct
  
  /** Finds all leaf terms of the specified multitype, regardless of how deep 
   * they are in the type hierarchy.
   */
  def findLeafTerms(x: XMultitype): List[XSchema] = (resolve(elementsOf(x)).flatMap {
    case x: XMultitype => findProductTerms(x)
    case x => x :: Nil
  }).distinct
  
  /** Attempts to find the definition for the specified reference.
   */
  def definitionFor(ref: XReference): Option[XDefinition] = ref match {
    case x: XDefinitionRef => definitions.find { y => y.name == x.name && y.namespace == x.namespace }
    
    case _ => None
  }
  
  /** Finds all the definitions referenced by the specified definition.
   */
  def definitionsReferencedBy(defn: XDefinition): List[XDefinitionRef] = walk(defn, Nil, new XSchemaWalker[List[XDefinitionRef]] {
    override def walk(data: List[XDefinitionRef], prim: XDefinitionRef): List[XDefinitionRef] = prim :: data
  }).distinct
  
  def definitionsReferencedIn(namespace: String): List[XDefinitionRef] = definitionsIn(namespace).flatMap(definitionsReferencedBy(_))
  
  lazy val products       = definitionsOfType(classOf[XProduct])  
  lazy val coproducts     = definitionsOfType(classOf[XCoproduct])  
  lazy val multitypes     = definitionsOfType(classOf[XMultitype])  
  lazy val unions         = definitionsOfType(classOf[XUnion])  
  lazy val references     = all.filter(referencesP).map(_.asInstanceOf[XReference])  
  lazy val definitionRefs = referencesOfType(classOf[XDefinitionRef])  
  lazy val primitiveRefs  = referencesOfType(classOf[XPrimitiveRef])
  
  def definitionsIn(namespace: String) = definitions.filter(_.namespace == namespace)  
  
  def constantsIn(namespace: String) = constants.filter(_.namespace == namespace)  
  
  def productsIn(namespace: String) = products.filter(_.namespace == namespace)  
  
  def unionsIn(namespace: String) = unions.filter(_.namespace == namespace)
  
  def multitypesIn(namespace: String) = multitypes.filter(_.namespace == namespace)
  
  def coproductsIn(namespace: String) = coproducts.filter(_.namespace == namespace)  
  
  def definitionRefsIn(namespace: String) = definitionRefs.filter(_.namespace == namespace)  
  
  def definitionByName(name: String) = definitions.filter(_.name == name).headOption
  
  /** Retrieves all the namespaces.
   */
  def namespaces = definitions.map(_.namespace).distinct

  /** Resolves the type. Will return the passed in type except when it is a 
   * reference to a defined type, in which case it will return the definition 
   * for the type.
   */
  def resolve(ref: XReference): XSchema = definitionFor(ref).getOrElse(ref)
  
  /** Resolves all types.
   */
  def resolve(refs: Iterable[XReference]): List[XSchema] = refs.map(resolve _).toList
  
  def referenceOf(x: XSchema): Option[XReference] = x match {
    case x: XDefinition => Some(x.referenceTo)
    case x: XReference  => Some(x)
    case _ => None
  }
  
  def referencesOf(xs: Iterable[XSchema]): List[XReference] = xs.toList.flatMap(referenceOf(_).toList)
  
  /** If the specified schema is a container for other types, returns those
   * types; otherwise, returns Nil.
   */
  def elementsOf(s: XSchema): List[XReference] = s match {
    case x: XProduct    => x.terms.map(_.fieldType)
    case x: XCoproduct  => x.terms
    case x: XUnion      => x.terms
    case x: XField      => x.fieldType :: Nil
    case x: XOptional   => x.optionalType :: Nil
    case x: XCollection => x.elementType :: Nil
    case x: XMap        => x.keyType :: x.valueType :: Nil
    case x: XTuple      => x.types
    case _ => Nil
  }
  
  /** Returns all the resolved containers of the specified definition.
   */
  def containersOf(defn: XDefinition) = definitions.filter { x =>
    elementsOf(x).contains(defn.referenceTo)
  }
  
  /** Returns all the coproducts that contain the specified definition.
   */
  def coproductContainersOf(defn: XDefinition): List[XCoproduct] = containersOf(defn).flatMap { 
    case x: XCoproduct => List[XCoproduct](x)
    case _ => Nil
  }
  
  /** Returns all the multitypes that contain the specified definition.
   */
  def multitypeContainersOf(defn: XDefinition): List[XMultitype] = containersOf(defn).flatMap { 
    case x: XMultitype => List[XMultitype](x)
    case _ => Nil
  }
  
  /** A "singleton" coproduct term is defined to be a product that appears as
   * a term in a single coproduct, and is not referenced as a type anywhere in
   * the type hierarchy. Singleton coproduct terms have special properties that
   * allow them to have natural representations in functional languages.
   */
  def isSingletonCoproductTerm(defn: XDefinition): Boolean = defn match {
    case x: XProduct => coproductContainersOf(defn) == 1 &&
      definitionRefs.filter(_ == x.referenceTo).length == 0
      
    case _ => false
  }
  
  /** Determines if the specified product is a term in any multitype.
   */
  def isContainedInMultitype(defn: XProduct) = multitypeContainersOf(defn).length > 0
  
  /** Returns the namespace of the reference, if it has one, or None if it 
    * does not.
    */
  def namespaceOf(ref: XReference) = ref match {
    case x: XDefinitionRef => Some(x.namespace)
    
    case _ => None
  }
  
  /** Retrieves all the namespaces of the terms of the specified multitype.
   */
  def namespacesOf(defn: XMultitype): List[String] = elementsOf(defn).flatMap(namespaceOf(_).toList)
  
  /** Compares the specificity of the specified references. Will return 0 for
   * two unrelated definitions. For related definitions, a definition X will
   * have greater specificity than a definition Y iff X is a refinement of Y.
   */
  def compareSpecificity(ref1: XReference, ref2: XReference): Int = {
    resolve(ref1) match {
      case x: XProduct => resolve(ref2) match {
        case y: XProduct => 0
        case y: XMultitype => if (multitypeContainersOf(x).contains(y)) 1 else 0
        
        case _ => 0
      }
      
      case x: XMultitype => resolve(ref2) match {
        case y: XProduct => if (multitypeContainersOf(y).contains(x)) -1 else 0
        case y: XMultitype => if (multitypeContainersOf(y).contains(x)) -1 else if (multitypeContainersOf(x).contains(y)) 1 else 0
        
        case _ => 0
      }
      
      case _ => 0
    }
  }
  
  /** Finds all fields that are common to all elements of the specified 
   * coproduct. Fields are common when they have the same name and type,
   * regardless of any other difference between them.
   */
  def commonFieldsOf(c: XCoproduct): List[(String, XReference)] = {
    val allFields: List[List[(String, XReference)]] = c.terms.map(resolve(_)).map { resolved => 
        resolved match {
          case p: XProduct => p.terms.flatMap { field =>
            resolve(field.fieldType) match {
              case p: XProduct if (isContainedInMultitype(p)) => (field.name, field.fieldType) :: multitypeContainersOf(p).map(c => (field.name, c.referenceTo))

              case _ => (field.name, field.fieldType) :: Nil
            }
          }
        
          case c: XCoproduct => commonFieldsOf(c)
          
          case _ => Nil
      }
    }
    
    (allFields match {
      case Nil => Nil
      
      case head :: tail => tail.foldLeft(head) { (common, list) => common.intersect(list) }
    }).sortWith { (t1, t2) =>
      val key1 = t1._1
      val key2 = t2._1
      
      if (key1 == key2) (compareSpecificity(t1._2, t2._2) > 0) else key1.compare(key2) < 0
    }.foldLeft[List[(String, XReference)]](Nil) { (list, e) => if (list.exists(_._1 == e._1)) list else e :: list }
  }


  
  def iterator = all.iterator
  
  lazy val all = definitions.flatMap { x => build(x) }
  
  private def build(s: XSchema): List[XSchema] = s :: elementsOf(s).flatMap(build _)
  
  private def constantsP = (x: XSchema) => x.isInstanceOf[XConstant]
  
  private def productsP = (x: XSchema) => x.isInstanceOf[XProduct]
  
  private def coproductsP = (x: XSchema) => x.isInstanceOf[XCoproduct]
  
  private def unionsP = (x: XSchema) => x.isInstanceOf[XUnion]
  
  private def referencesP = (x: XSchema) => x.isInstanceOf[XReference]
  
  private def definitionRefsP = (x: XSchema) => x.isInstanceOf[XDefinitionRef]
  
  private def primitiveRefsP = (x: XSchema) => x.isInstanceOf[XPrimitiveRef]
  
  private def definitionsOfType[T <: XDefinition](c: Class[T]): List[T] = definitions.filter { x => c.isAssignableFrom(x.getClass) }.map(_.asInstanceOf[T])
  
  private def referencesOfType[T <: XReference](c: Class[T]): List[T] = references.filter { x => c.isAssignableFrom(x.getClass) }.map(_.asInstanceOf[T])
}

object XSchemaDatabase {
  import blueeyes.json.JsonAST._
  import blueeyes.json.JsonParser._
  import blueeyes.json.xschema.Serialization._
  
  def apply(d: List[XDefinition], c: List[XConstant]): XSchemaDatabase = new XSchemaDatabase {
    val definitions = d
    
    val constants = c
  }
  
  def apply(root: XRoot): XSchemaDatabase = apply(root.definitions, root.constants)
  
  def apply(json: JValue): XSchemaDatabase = apply(json.deserialize[XRoot])
  
  def apply(str: String): XSchemaDatabase = apply(parse(str))
}

}