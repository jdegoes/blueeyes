package blueeyes.json.serialization {

import _root_.blueeyes.json.JsonAST._
import _root_.blueeyes.json.JsonParser.{parse => j}

trait SchemaHelpers {
  protected def array(s: String*) = JArray(s.toList.map(JString(_)))
  protected def string(s: String) = JString(s)
}

class BootstrapXSchema extends SchemaHelpers {
  def apply: XRoot = XRoot(
    List(
      XProduct(
        "XRoot", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("definitions", Map(), XList(XDefinitionRef("XDefinition", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending),
          XRealField("constants",   Map(), XList(XDefinitionRef("XConstant", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XSchema", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XDefinition", "blueeyes.json.serialization"),
          XDefinitionRef("XReference",  "blueeyes.json.serialization"),
          XDefinitionRef("XField",      "blueeyes.json.serialization"),
          XDefinitionRef("XConstant",   "blueeyes.json.serialization")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XReference", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XPrimitiveRef",  "blueeyes.json.serialization"),
          XDefinitionRef("XContainerRef",  "blueeyes.json.serialization"),
          XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XPrimitiveRef", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XBoolean", "blueeyes.json.serialization"),
          XDefinitionRef("XInt",     "blueeyes.json.serialization"),
          XDefinitionRef("XLong",    "blueeyes.json.serialization"),
          XDefinitionRef("XFloat",   "blueeyes.json.serialization"),
          XDefinitionRef("XDouble",  "blueeyes.json.serialization"),
          XDefinitionRef("XString",  "blueeyes.json.serialization"),
          XDefinitionRef("XJSON",    "blueeyes.json.serialization"),
          XDefinitionRef("XDate",    "blueeyes.json.serialization")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XContainerRef", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XCollection", "blueeyes.json.serialization"),
          XDefinitionRef("XMap",        "blueeyes.json.serialization"),
          XDefinitionRef("XOptional",   "blueeyes.json.serialization"),
          XDefinitionRef("XTuple",      "blueeyes.json.serialization")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct(
        "XDefinitionRef", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("name", Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending)
        )
      ),
      XProduct("XBoolean", "blueeyes.json.serialization", Map(), List()),
      XProduct("XInt",     "blueeyes.json.serialization", Map(), List()),
      XProduct("XLong",    "blueeyes.json.serialization", Map(), List()),
      XProduct("XFloat",   "blueeyes.json.serialization", Map(), List()),
      XProduct("XDouble",  "blueeyes.json.serialization", Map(), List()),
      XProduct("XString",  "blueeyes.json.serialization", Map(), List()),
      XProduct("XJSON",    "blueeyes.json.serialization", Map(), List()),
      XProduct("XDate",    "blueeyes.json.serialization", Map(), List()),
      XCoproduct(
        "XCollection", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XList", "blueeyes.json.serialization"),
          XDefinitionRef("XSet", "blueeyes.json.serialization"),
          XDefinitionRef("XArray", "blueeyes.json.serialization")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct("XList", "blueeyes.json.serialization", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XSet", "blueeyes.json.serialization", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XArray", "blueeyes.json.serialization", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XMap", "blueeyes.json.serialization", Map(),
        List(
          XRealField("keyType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending),
          XRealField("valueType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct("XOptional", "blueeyes.json.serialization", Map(),
        List(XRealField("optionalType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XTuple", "blueeyes.json.serialization", Map(),
        List(XRealField("types", Map(), XList(XDefinitionRef("XReference", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending))
      ),
      XCoproduct(
        "XDefinition", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XProduct", "blueeyes.json.serialization"),
          XDefinitionRef("XMultitype", "blueeyes.json.serialization")
        ),
        j(""" { "XProduct": {} } """)
      ),
      XCoproduct(
        "XMultitype", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XCoproduct", "blueeyes.json.serialization"),
          XDefinitionRef("XUnion", "blueeyes.json.serialization")
        ),
        j(""" { "XCoproduct": {} } """)
      ),
      XCoproduct(
        "XField", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XRealField", "blueeyes.json.serialization"),
          XDefinitionRef("XViewField", "blueeyes.json.serialization"),
          XDefinitionRef("XConstantField", "blueeyes.json.serialization")
        ),
        j(""" { "XRealField": {} } """)
      ),
      XProduct(
        "XProduct", "blueeyes.json.serialization",
        Map(
          "scala.class.traits" -> string("blueeyes.json.serialization.XProductBehavior"),
          "xschema.doc" -> string("""A product is analogous to a record: it contains fields, which may be
                              any type, have default values, and have a user-defined ordering.
                              Products are the fundamental building blocks used to construct most 
                              data structures.""")
        ),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XField", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization"))
        )
      ),
      XProduct(
        "XCoproduct", "blueeyes.json.serialization",
        Map(
          "xschema.doc" -> string("""A coproduct is a data structure that can assume one of N other types. 
                              These types must be either products, or other coproducts -- primitives
                              are not allowed because they cannot be mapped cleanly to most languages
                              (see unions for a disjoint structure that allows primitives). <p>
                              Note that most languages cannot handle coproducts of unions.
                              """)
        ),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending),
          XRealField("default",     Map(), XJSON, JNothing, XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization"))
        )
      ),
      XProduct(
        "XUnion", "blueeyes.json.serialization",
        Map(
          "xschema.doc" -> string("""A union is a C-style union of N types -- referred to as terms. Unlike 
                              coproducts, unions have no effect on the type hierarchy of the specified 
                              terms, and the terms may include primitive types, in addition to references
                              to products, coproducts, and other unions. Although unions have names and 
                              namespaces, most languages do not have explicit support for union types, 
                              and in such cases, no entity will be generated for them; they will be 
                              translated into the supertype of all the terms. <p>Some code generators 
                              may not be able to handle unions or coproducts that contain unions.""")
        ),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XReference", "blueeyes.json.serialization")), j("""[]"""), XOrderAscending),
          XRealField("default",     Map(), XJSON, JNothing, XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization"))
        )
      ),
      XProduct(
        "XConstant", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("name",         Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",    Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",   Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("constantType", Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",      Map(), XJSON, JString(""), XOrderAscending),
          
          XViewField("referenceTo",  Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.serialization"))
        )
      ),
      XProduct(
        "XRealField", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), JArray(Nil), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending),
          XRealField("order",      Map(), XDefinitionRef("XOrder", "blueeyes.json.serialization"), j(""" { "XOrderAscending": {} } """), XOrderAscending)
        )
      ),
      XProduct(
        "XViewField", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct(
        "XConstantField", "blueeyes.json.serialization",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.serialization"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XOrder", "blueeyes.json.serialization",
        Map(),
        List(
          XDefinitionRef("XOrderAscending",  "blueeyes.json.serialization"),
          XDefinitionRef("XOrderDescending", "blueeyes.json.serialization"),
          XDefinitionRef("XOrderIgnore",     "blueeyes.json.serialization")
        ),
        j(""" { "XOrderAscending": {} } """)
      ),
      XProduct("XOrderAscending",  "blueeyes.json.serialization", Map(), List()),
      XProduct("XOrderDescending", "blueeyes.json.serialization", Map(), List()),
      XProduct("XOrderIgnore",     "blueeyes.json.serialization", Map(), List())
    ),
    Nil,
    Map(
    )
  )
}

object SampleSchemas extends SchemaHelpers {
  val DataSocialGenderSchema = XRoot(
    List(
      XCoproduct(
        "Gender", "data.social",
        Map(
          "xschema.doc" -> string("This is the coproduct that includes male and female. The normal way to translate this into OOP is as a superclass/superinterface."),
          "scala.class.traits" -> array("java.io.Serializable", "java.lang.Cloneable"),
          "scala.object.traits" -> array("java.io.Serializable", "java.lang.Cloneable")
        ),
        List(
          XDefinitionRef("Male", "data.social"),
          XDefinitionRef("Female", "data.social")
        ),
        j(""" { "Male": { "text": "foo" } } """)
      ),
      XProduct(
        "Male", "data.social",
        Map("scala.class.traits" -> array("java.io.Serializable", "java.lang.Cloneable")),
        List(
          XRealField("text", Map(), XString, JString("male"), XOrderDescending),
          XViewField("asFemale", Map(), XDefinitionRef("Female", "data.social"))
        )
      ),
      XProduct(
        "Female", "data.social",
        Map("scala.class.traits" -> array("java.io.Serializable", "java.lang.Cloneable")),
        List(
          XRealField("text", Map(), XString, JString("female"), XOrderAscending),
          XViewField("asMale", Map(), XDefinitionRef("Male", "data.social"))
        )
      ),
      XProduct(
        "Morning", "data.social",
        Map(),
        List()
      ),
      XProduct(
        "Noon", "data.social",
        Map(),
        List()
      ),
      XProduct(
        "Night", "data.social",
        Map(),
        List()
      ),
      XCoproduct(
        "Time", "data.social",
        Map(),
        List(
          XDefinitionRef("Morning", "data.social"),
          XDefinitionRef("Noon", "data.social"),
          XDefinitionRef("Night", "data.social")
        ),
        j(""" { "Morning": {} } """)
      )
    ),
    List(
      XConstant(
        "DefaultFemale", "data.social",
        Map(),
        XDefinitionRef("Gender", "data.social"),
        JObject(
          JField("Female",
            JObject(
              JField("text", JString("female")) :: Nil
            )
          ) :: Nil
        )
      ),
      XConstant(
        "DefaultMale", "data.social",
        Map(),
        XDefinitionRef("Gender", "data.social"),
        JObject(
          JField("Male",
            JObject(
              JField("text", JString("male")) :: Nil
            )
          ) :: Nil
        )
      )
    ),
    Map(
      "scala.imports" -> array("blueeyes.json.serialization.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}", "java.lang.reflect._")
    )
  )
  
  val EmployeeSchema = XRoot(
    List(
      XCoproduct(
        "Employee", "data.employee",
        Map(),
        List(
          XDefinitionRef("Manager", "data.employee"),
          XDefinitionRef("Secretary", "data.employee"),
          XDefinitionRef("Coach", "data.employee")
        ),
        j(""" { "Manager": {} } """)
      ),
      XCoproduct(
        "ID", "data.employee",
        Map(),
        List(
          XDefinitionRef("EmployeeID", "data.employee"),
          XDefinitionRef("NoID", "data.employee")
        ),
        j(""" { "NoID": {} } """)
      ),
      XCoproduct(
        "EmployeeID", "data.employee",
        Map(),
        List(
          XDefinitionRef("SSN", "data.employee"),
          XDefinitionRef("Passport", "data.employee"),
          XDefinitionRef("DL", "data.employee")
        ),
        j(""" { "SSN": {} } """)
      ),
      XProduct(
        "NoID", "data.employee",
        Map(),
        List()
      ),
      XProduct(
        "SSN", "data.employee",
        Map(),
        List(
          XRealField("value", Map(), XString, JString(""), XOrderAscending)
        )
      ),
      XProduct(
        "Passport", "data.employee",
        Map(),
        List(
          XRealField("value", Map(), XLong, JInt(-1), XOrderAscending)
        )
      ),
      XProduct(
        "DL", "data.employee",
        Map(),
        List(
          XRealField("value", Map(), XString, JString(""), XOrderAscending)
        )
      ),
      XProduct(
        "Manager", "data.employee",
        Map(),
        List(
          XRealField("id", Map(), XDefinitionRef("SSN", "data.employee"), j(""" { "SSN": {} } """), XOrderAscending)
        )
      ),
      XProduct(
        "Secretary", "data.employee",
        Map(),
        List(
          XRealField("id", Map(), XDefinitionRef("SSN", "data.employee"), j(""" { "SSN": {} } """), XOrderAscending)
        )
      ),
      XProduct(
        "Coach", "data.employee",
        Map(),
        List(
          XRealField("id", Map(), XDefinitionRef("SSN", "data.employee"), j(""" { "SSN": {} } """), XOrderAscending)
        )
      )
    ),
    Nil,
    Map(
      "scala.imports" -> array("blueeyes.json.serialization.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}", "java.lang.reflect._")
    )
  )
  
  val FringeFeaturesSchema = XRoot(
    XProduct(
      "ConstantSingleton", "data.fringe",
      Map(),
      List(
        XConstantField("constantNumber", Map(), XInt,     JInt(123)),
        XConstantField("constantString", Map(), XString,  JString("foo")),
        XConstantField("constantBool",   Map(), XBoolean, JString("foo")),
        XConstantField("constantDate",   Map(), XDate,    JInt(275296173))
      )
    ) :: 
    XProduct(
      "ConstantWithRealField", "data.fringe",
      Map(),
      List(
        XRealField("value", Map(), XLong, JInt(-1), XOrderAscending),
        XConstantField("constantNumber", Map(), XInt,     JInt(123)),
        XConstantField("constantString", Map(), XString,  JString("foo")),
        XConstantField("constantBool",   Map(), XBoolean, JBool(true)),
        XConstantField("constantDate",   Map(), XDate,    JInt(275296173))
      )
    ) ::
    XUnion(
      "UnionOfStringAndProduct", "data.fringe",
      Map(),
      List(
        XString,
        XDefinitionRef("ConstantWithRealField", "data.fringe")
      ),
      j(""" { "String": {} } """)
    ) ::
    XProduct(
      "ProductWithUnionField", "data.fringe",
      Map(),
      List(
        XRealField("value", Map(), XDefinitionRef("UnionOfStringAndProduct", "data.fringe"), j(""" { "String": "" } """), XOrderAscending),
        XRealField("stringMap", Map(), XMap(XString, XString), j(""" { "foo": "bar" } """), XOrderAscending)
      )
    ) :: Nil,
    XConstant(
      "ConstantBool", "data.fringe",
      Map(),
      XBoolean,
      JBool(true)
    ) :: Nil,
    Map()
  )
  
  val XSchemaSchema = (new BootstrapXSchema).apply
}
}













// This code was auto-generated by Lift Json XSchema - do not edit
package data.fringe {
  import blueeyes.json.JsonParser._
  import blueeyes.json.JsonAST._
  import blueeyes.json.serialization.{SerializationImplicits, Extractor, Decomposer}
  import blueeyes.json.serialization.DefaultOrderings._
  
  
  trait Orderings {
    implicit val ConstantSingletonOrdering: Ordering[data.fringe.ConstantSingleton.type] = new Ordering[data.fringe.ConstantSingleton.type] {
      def compare(v1: data.fringe.ConstantSingleton.type, v2: data.fringe.ConstantSingleton.type): Int = {
        import Stream.{cons, empty}
        
        return if (v1 == v2) 0 else {      
          val comparisons = empty
          
          comparisons.dropWhile(_ == 0).append(0 :: Nil).head
        }
      }
    }
    
    implicit val ConstantWithRealFieldOrdering: Ordering[data.fringe.ConstantWithRealField] = new Ordering[data.fringe.ConstantWithRealField] {
      def compare(v1: data.fringe.ConstantWithRealField, v2: data.fringe.ConstantWithRealField): Int = {
        import Stream.{cons, empty}
        
        return if (v1 == v2) 0 else {      
          val comparisons = cons(blueeyes.json.serialization.DefaultOrderings.LongOrdering.compare(v1.value, v2.value) * 1, empty)
          
          comparisons.dropWhile(_ == 0).append(0 :: Nil).head
        }
      }
    }
    
    val UnionOfStringAndProductOrdering: Ordering[Any] = new Ordering[Any] {
      def compare(v1: Any, v2: Any): Int = {
        if (v1 == v2) 0
        else v1 match {
          case x: String => v2 match {
            case y: String => blueeyes.json.serialization.DefaultOrderings.StringOrdering.compare(x, y)
            case y: data.fringe.ConstantWithRealField => -1
          }
          case x: data.fringe.ConstantWithRealField => v2 match {
            case y: String => 1
            case y: data.fringe.ConstantWithRealField => data.fringe.Orderings.ConstantWithRealFieldOrdering.compare(x, y)
          }
        }
      }
    }
    
    implicit val ProductWithUnionFieldOrdering: Ordering[data.fringe.ProductWithUnionField] = new Ordering[data.fringe.ProductWithUnionField] {
      def compare(v1: data.fringe.ProductWithUnionField, v2: data.fringe.ProductWithUnionField): Int = {
        import Stream.{cons, empty}
        
        return if (v1 == v2) 0 else {      
          val comparisons = cons(data.fringe.Orderings.UnionOfStringAndProductOrdering.compare(v1.value, v2.value) * 1, cons(blueeyes.json.serialization.DefaultOrderings.MapOrdering(blueeyes.json.serialization.DefaultOrderings.StringOrdering, blueeyes.json.serialization.DefaultOrderings.StringOrdering).compare(v1.stringMap, v2.stringMap) * 1, empty))
          
          comparisons.dropWhile(_ == 0).append(0 :: Nil).head
        }
      }
    }
  }
  object Orderings extends Orderings
  
  case object ConstantSingleton 
  
  case class ConstantWithRealField(value: Long) extends Ordered[data.fringe.ConstantWithRealField] {
    def compare(that: data.fringe.ConstantWithRealField): Int = data.fringe.Orderings.ConstantWithRealFieldOrdering.compare(this, that)
    
  }
  
  case class ProductWithUnionField(value: Any, stringMap: Map[String, String]) extends Ordered[data.fringe.ProductWithUnionField] {
    def compare(that: data.fringe.ProductWithUnionField): Int = data.fringe.Orderings.ProductWithUnionFieldOrdering.compare(this, that)
    
  }
  
  trait Extractors {
    protected def extractField[T](jvalue: JValue, name: String, default: JValue, e: Extractor[T]): T = {
      try {
        e.extract((jvalue \ name).getOrElse(default))
      }
      catch {
        case _ => e.extract(default)
      }
    }
    
    implicit val ConstantSingletonExtractor: Extractor[data.fringe.ConstantSingleton.type] = new Extractor[data.fringe.ConstantSingleton.type] {
      def extract(jvalue: JValue): data.fringe.ConstantSingleton.type = {
        ConstantSingleton
      }
    }
    
    implicit val ConstantWithRealFieldExtractor: Extractor[data.fringe.ConstantWithRealField] = new Extractor[data.fringe.ConstantWithRealField] {
      def extract(jvalue: JValue): data.fringe.ConstantWithRealField = {
        ConstantWithRealField(
          extractField[Long](jvalue, "value", JInt(-1), blueeyes.json.serialization.DefaultExtractors.LongExtractor)
        )
      }
    }
    
    lazy val UnionOfStringAndProductExtractorFunction: PartialFunction[JField, Any] = ({
      case JField("String", value) => blueeyes.json.serialization.DefaultExtractors.StringExtractor.extract(value)
      case JField("ConstantWithRealField", value) => data.fringe.Extractors.ConstantWithRealFieldExtractor.extract(value)
    }: PartialFunction[JField, Any])
    val UnionOfStringAndProductExtractor: Extractor[Any] = new Extractor[Any] {
      def extract(jvalue: JValue): Any = {
        def extract0(jvalue: JValue): Option[Any] = {
          (jvalue --> classOf[JObject]).fields.filter(UnionOfStringAndProductExtractorFunction.isDefinedAt _) match {
            case field :: fields => Some(UnionOfStringAndProductExtractorFunction(field))
            case Nil => None
          }
        }
        
        extract0(jvalue) match {
          case Some(v) => v
          case None => extract0(JObject(JField("String",JObject(Nil))::Nil)) match {
            case Some(v) => v
            case None => sys.error("Expected to find Any, but found " + jvalue + ", and default value was invalid")
          }
        }
      }
    }
    
    implicit val ProductWithUnionFieldExtractor: Extractor[data.fringe.ProductWithUnionField] = new Extractor[data.fringe.ProductWithUnionField] {
      def extract(jvalue: JValue): data.fringe.ProductWithUnionField = {
        ProductWithUnionField(
          extractField[Any](jvalue, "value", JObject(JField("String",JString(""))::Nil), data.fringe.Extractors.UnionOfStringAndProductExtractor),
          extractField[Map[String, String]](jvalue, "stringMap", JObject(JField("foo",JString("bar"))::Nil), blueeyes.json.serialization.DefaultExtractors.StringMapExtractor(blueeyes.json.serialization.DefaultExtractors.StringExtractor))
        )
      }
    }
  }
  object Extractors extends Extractors
  
  trait Decomposers {
    implicit val ConstantSingletonDecomposer: Decomposer[data.fringe.ConstantSingleton.type] = new Decomposer[data.fringe.ConstantSingleton.type] {
      def decompose(tvalue: data.fringe.ConstantSingleton.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val ConstantWithRealFieldDecomposer: Decomposer[data.fringe.ConstantWithRealField] = new Decomposer[data.fringe.ConstantWithRealField] {
      def decompose(tvalue: data.fringe.ConstantWithRealField): JValue = {
        JObject(
          JField("value", blueeyes.json.serialization.DefaultDecomposers.LongDecomposer.decompose(tvalue.value)) :: Nil
        )
      }
    }
    
    val UnionOfStringAndProductDecomposer: Decomposer[Any] = new Decomposer[Any] {
      def decompose(tvalue: Any): JValue = {
        tvalue match {
          case x: String => JObject(JField("String", blueeyes.json.serialization.DefaultDecomposers.StringDecomposer.decompose(x)) :: Nil)
          case x: data.fringe.ConstantWithRealField => JObject(JField("ConstantWithRealField", data.fringe.Decomposers.ConstantWithRealFieldDecomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val ProductWithUnionFieldDecomposer: Decomposer[data.fringe.ProductWithUnionField] = new Decomposer[data.fringe.ProductWithUnionField] {
      def decompose(tvalue: data.fringe.ProductWithUnionField): JValue = {
        JObject(
          JField("value", data.fringe.Decomposers.UnionOfStringAndProductDecomposer.decompose(tvalue.value)) ::
          JField("stringMap", blueeyes.json.serialization.DefaultDecomposers.StringMapDecomposer(blueeyes.json.serialization.DefaultDecomposers.StringDecomposer).decompose(tvalue.stringMap)) :: Nil
        )
      }
    }
  }
  object Decomposers extends Decomposers
  
  object Serialization extends Decomposers with Extractors with SerializationImplicits {
    
  }
  
  object Constants {
    import Serialization._
    
    lazy val ConstantBool: Boolean = blueeyes.json.serialization.DefaultExtractors.BooleanExtractor.extract(JBool(true))
  }
}// These tests were auto-generated by Lift Json XSchema - do not edit
package data.fringe {
  import _root_.org.specs2.mutable.Specification

  import blueeyes.json.JsonParser._
  import blueeyes.json.JsonAST._
  
  import blueeyes.json.serialization.DefaultSerialization._
  
  import data.fringe.Serialization._
  import data.fringe.Constants._

  object ExampleProductData {
    lazy val ExampleConstantSingleton: data.fringe.ConstantSingleton.type = data.fringe.Extractors.ConstantSingletonExtractor.extract(JObject(Nil))
    
    lazy val ExampleConstantWithRealField: data.fringe.ConstantWithRealField = JObject(Nil).deserialize[data.fringe.ConstantWithRealField]
    
    lazy val ExampleProductWithUnionField: data.fringe.ProductWithUnionField = JObject(Nil).deserialize[data.fringe.ProductWithUnionField]
  }
  object DataProductSerializationExamples extends Specification {
    override def is = args(sequential = true) ^ super.is
    "Deserialization of ConstantSingleton succeeds even when information is missing" in {
      ExampleProductData.ExampleConstantSingleton.isInstanceOf[data.fringe.ConstantSingleton.type] must be_==(true)
    }
    "Serialization of ConstantSingleton has non-zero information content" in {
      Decomposers.ConstantSingletonDecomposer.decompose(ExampleProductData.ExampleConstantSingleton) must not (be (JObject(Nil)))
    }
    
    "Deserialization of ConstantWithRealField succeeds even when information is missing" in {
      ExampleProductData.ExampleConstantWithRealField.isInstanceOf[data.fringe.ConstantWithRealField] must be_==(true)
    }
    "Serialization of ConstantWithRealField has non-zero information content" in {
      ExampleProductData.ExampleConstantWithRealField.serialize must not be (JObject(Nil))
    }
  
    
    "Deserialization of ProductWithUnionField succeeds even when information is missing" in {
      ExampleProductData.ExampleProductWithUnionField.isInstanceOf[data.fringe.ProductWithUnionField] must be_==(true)
    }
    "Serialization of ProductWithUnionField has non-zero information content" in {
      ExampleProductData.ExampleProductWithUnionField.serialize must not be (JObject(Nil))
    }
  
  }
  object ExampleMultitypeData {
    
  }
  object DataCoproductSerializationExamples extends Specification {
    
  }
  object DataConstantsSerializationExamples extends Specification {
    "Deserialization of constant ConstantBool succeeds" in {
      Constants.ConstantBool.serialize.deserialize[Boolean] must be_== (Constants.ConstantBool)
    }
    "Serialization of constant ConstantBool has non-zero information content" in {
      Constants.ConstantBool.serialize must_!= (JObject(Nil))
    }
  
  }
}

