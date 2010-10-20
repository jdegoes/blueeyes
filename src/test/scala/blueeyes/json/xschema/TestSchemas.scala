package blueeyes.json.xschema {

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
        "XRoot", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("definitions", Map(), XList(XDefinitionRef("XDefinition", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("constants",   Map(), XList(XDefinitionRef("XConstant", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XSchema", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XDefinition", "blueeyes.json.xschema"),
          XDefinitionRef("XReference",  "blueeyes.json.xschema"),
          XDefinitionRef("XField",      "blueeyes.json.xschema"),
          XDefinitionRef("XConstant",   "blueeyes.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XReference", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XPrimitiveRef",  "blueeyes.json.xschema"),
          XDefinitionRef("XContainerRef",  "blueeyes.json.xschema"),
          XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XPrimitiveRef", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XBoolean", "blueeyes.json.xschema"),
          XDefinitionRef("XInt",     "blueeyes.json.xschema"),
          XDefinitionRef("XLong",    "blueeyes.json.xschema"),
          XDefinitionRef("XFloat",   "blueeyes.json.xschema"),
          XDefinitionRef("XDouble",  "blueeyes.json.xschema"),
          XDefinitionRef("XString",  "blueeyes.json.xschema"),
          XDefinitionRef("XJSON",    "blueeyes.json.xschema"),
          XDefinitionRef("XDate",    "blueeyes.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XContainerRef", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XCollection", "blueeyes.json.xschema"),
          XDefinitionRef("XMap",        "blueeyes.json.xschema"),
          XDefinitionRef("XOptional",   "blueeyes.json.xschema"),
          XDefinitionRef("XTuple",      "blueeyes.json.xschema")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct(
        "XDefinitionRef", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending)
        )
      ),
      XProduct("XBoolean", "blueeyes.json.xschema", Map(), List()),
      XProduct("XInt",     "blueeyes.json.xschema", Map(), List()),
      XProduct("XLong",    "blueeyes.json.xschema", Map(), List()),
      XProduct("XFloat",   "blueeyes.json.xschema", Map(), List()),
      XProduct("XDouble",  "blueeyes.json.xschema", Map(), List()),
      XProduct("XString",  "blueeyes.json.xschema", Map(), List()),
      XProduct("XJSON",    "blueeyes.json.xschema", Map(), List()),
      XProduct("XDate",    "blueeyes.json.xschema", Map(), List()),
      XCoproduct(
        "XCollection", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XList", "blueeyes.json.xschema"),
          XDefinitionRef("XSet", "blueeyes.json.xschema"),
          XDefinitionRef("XArray", "blueeyes.json.xschema")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct("XList", "blueeyes.json.xschema", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XSet", "blueeyes.json.xschema", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XArray", "blueeyes.json.xschema", Map(),
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XMap", "blueeyes.json.xschema", Map(),
        List(
          XRealField("keyType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending),
          XRealField("valueType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct("XOptional", "blueeyes.json.xschema", Map(),
        List(XRealField("optionalType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XTuple", "blueeyes.json.xschema", Map(),
        List(XRealField("types", Map(), XList(XDefinitionRef("XReference", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending))
      ),
      XCoproduct(
        "XDefinition", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XProduct", "blueeyes.json.xschema"),
          XDefinitionRef("XMultitype", "blueeyes.json.xschema")
        ),
        j(""" { "XProduct": {} } """)
      ),
      XCoproduct(
        "XMultitype", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XCoproduct", "blueeyes.json.xschema"),
          XDefinitionRef("XUnion", "blueeyes.json.xschema")
        ),
        j(""" { "XCoproduct": {} } """)
      ),
      XCoproduct(
        "XField", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XRealField", "blueeyes.json.xschema"),
          XDefinitionRef("XViewField", "blueeyes.json.xschema"),
          XDefinitionRef("XConstantField", "blueeyes.json.xschema")
        ),
        j(""" { "XRealField": {} } """)
      ),
      XProduct(
        "XProduct", "blueeyes.json.xschema",
        Map(
          "scala.class.traits" -> string("blueeyes.json.xschema.XProductBehavior"),
          "xschema.doc" -> string("""A product is analogous to a record: it contains fields, which may be
                              any type, have default values, and have a user-defined ordering.
                              Products are the fundamental building blocks used to construct most 
                              data structures.""")
        ),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XField", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema"))
        )
      ),
      XProduct(
        "XCoproduct", "blueeyes.json.xschema",
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
          XRealField("terms",       Map(), XList(XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("default",     Map(), XJSON, JNothing, XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema"))
        )
      ),
      XProduct(
        "XUnion", "blueeyes.json.xschema",
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
          XRealField("terms",       Map(), XList(XDefinitionRef("XReference", "blueeyes.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("default",     Map(), XJSON, JNothing, XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema"))
        )
      ),
      XProduct(
        "XConstant", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("name",         Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",    Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",   Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("constantType", Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",      Map(), XJSON, JString(""), XOrderAscending),
          
          XViewField("referenceTo",  Map(), XDefinitionRef("XDefinitionRef", "blueeyes.json.xschema"))
        )
      ),
      XProduct(
        "XRealField", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), JArray(Nil), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending),
          XRealField("order",      Map(), XDefinitionRef("XOrder", "blueeyes.json.xschema"), j(""" { "XOrderAscending": {} } """), XOrderAscending)
        )
      ),
      XProduct(
        "XViewField", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct(
        "XConstantField", "blueeyes.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XJSON), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "blueeyes.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XOrder", "blueeyes.json.xschema",
        Map(),
        List(
          XDefinitionRef("XOrderAscending",  "blueeyes.json.xschema"),
          XDefinitionRef("XOrderDescending", "blueeyes.json.xschema"),
          XDefinitionRef("XOrderIgnore",     "blueeyes.json.xschema")
        ),
        j(""" { "XOrderAscending": {} } """)
      ),
      XProduct("XOrderAscending",  "blueeyes.json.xschema", Map(), List()),
      XProduct("XOrderDescending", "blueeyes.json.xschema", Map(), List()),
      XProduct("XOrderIgnore",     "blueeyes.json.xschema", Map(), List())
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
      "scala.imports" -> array("blueeyes.json.xschema.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}", "java.lang.reflect._")
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
      "scala.imports" -> array("blueeyes.json.xschema.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}", "java.lang.reflect._")
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
  import blueeyes.json.xschema.{SerializationImplicits, Extractor, Decomposer}
  import blueeyes.json.xschema.DefaultOrderings._
  
  
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
          val comparisons = cons(blueeyes.json.xschema.DefaultOrderings.LongOrdering.compare(v1.value, v2.value) * 1, empty)
          
          comparisons.dropWhile(_ == 0).append(0 :: Nil).head
        }
      }
    }
    
    val UnionOfStringAndProductOrdering: Ordering[Any] = new Ordering[Any] {
      def compare(v1: Any, v2: Any): Int = {
        if (v1 == v2) 0
        else v1 match {
          case x: String => v2 match {
            case y: String => blueeyes.json.xschema.DefaultOrderings.StringOrdering.compare(x, y)
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
          val comparisons = cons(data.fringe.Orderings.UnionOfStringAndProductOrdering.compare(v1.value, v2.value) * 1, cons(blueeyes.json.xschema.DefaultOrderings.MapOrdering(blueeyes.json.xschema.DefaultOrderings.StringOrdering, blueeyes.json.xschema.DefaultOrderings.StringOrdering).compare(v1.stringMap, v2.stringMap) * 1, empty))
          
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
        e.extract((jvalue \ name -->? classOf[JField]).map(_.value).getOrElse(default))
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
          extractField[Long](jvalue, "value", JInt(-1), blueeyes.json.xschema.DefaultExtractors.LongExtractor)
        )
      }
    }
    
    lazy val UnionOfStringAndProductExtractorFunction: PartialFunction[JField, Any] = ({
      case JField("String", value) => blueeyes.json.xschema.DefaultExtractors.StringExtractor.extract(value)
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
            case None => error("Expected to find Any, but found " + jvalue + ", and default value was invalid")
          }
        }
      }
    }
    
    implicit val ProductWithUnionFieldExtractor: Extractor[data.fringe.ProductWithUnionField] = new Extractor[data.fringe.ProductWithUnionField] {
      def extract(jvalue: JValue): data.fringe.ProductWithUnionField = {
        ProductWithUnionField(
          extractField[Any](jvalue, "value", JObject(JField("String",JString(""))::Nil), data.fringe.Extractors.UnionOfStringAndProductExtractor),
          extractField[Map[String, String]](jvalue, "stringMap", JObject(JField("foo",JString("bar"))::Nil), blueeyes.json.xschema.DefaultExtractors.StringMapExtractor(blueeyes.json.xschema.DefaultExtractors.StringExtractor))
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
          JField("value", blueeyes.json.xschema.DefaultDecomposers.LongDecomposer.decompose(tvalue.value)) :: Nil
        )
      }
    }
    
    val UnionOfStringAndProductDecomposer: Decomposer[Any] = new Decomposer[Any] {
      def decompose(tvalue: Any): JValue = {
        tvalue match {
          case x: String => JObject(JField("String", blueeyes.json.xschema.DefaultDecomposers.StringDecomposer.decompose(x)) :: Nil)
          case x: data.fringe.ConstantWithRealField => JObject(JField("ConstantWithRealField", data.fringe.Decomposers.ConstantWithRealFieldDecomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val ProductWithUnionFieldDecomposer: Decomposer[data.fringe.ProductWithUnionField] = new Decomposer[data.fringe.ProductWithUnionField] {
      def decompose(tvalue: data.fringe.ProductWithUnionField): JValue = {
        JObject(
          JField("value", data.fringe.Decomposers.UnionOfStringAndProductDecomposer.decompose(tvalue.value)) ::
          JField("stringMap", blueeyes.json.xschema.DefaultDecomposers.StringMapDecomposer(blueeyes.json.xschema.DefaultDecomposers.StringDecomposer).decompose(tvalue.stringMap)) :: Nil
        )
      }
    }
  }
  object Decomposers extends Decomposers
  
  object Serialization extends Decomposers with Extractors with SerializationImplicits {
    
  }
  
  object Constants {
    import Serialization._
    
    lazy val ConstantBool: Boolean = blueeyes.json.xschema.DefaultExtractors.BooleanExtractor.extract(JBool(true))
  }
}// These tests were auto-generated by Lift Json XSchema - do not edit
package data.fringe {
  import _root_.org.specs.Specification
  import _root_.org.specs.runner.{Runner, JUnit}
  
  import blueeyes.json.JsonParser._
  import blueeyes.json.JsonAST._
  
  import blueeyes.json.xschema.DefaultSerialization._
  
  import data.fringe.Serialization._
  import data.fringe.Constants._

  import data.fringe.{ConstantSingleton, ConstantWithRealField, ProductWithUnionField}
  
  object ExampleProductData {
    lazy val ExampleConstantSingleton: data.fringe.ConstantSingleton.type = data.fringe.Extractors.ConstantSingletonExtractor.extract(JObject(Nil))
    
    lazy val ExampleConstantWithRealField: data.fringe.ConstantWithRealField = JObject(Nil).deserialize[data.fringe.ConstantWithRealField]
    
    lazy val ExampleProductWithUnionField: data.fringe.ProductWithUnionField = JObject(Nil).deserialize[data.fringe.ProductWithUnionField]
  }
  class DataProductSerializationTest extends Runner(DataProductSerializationExamples) with JUnit
  object DataProductSerializationExamples extends Specification {
    "Deserialization of ConstantSingleton succeeds even when information is missing" in {
      ExampleProductData.ExampleConstantSingleton.isInstanceOf[data.fringe.ConstantSingleton.type] must be (true)
    }
    "Serialization of ConstantSingleton has non-zero information content" in {
      Decomposers.ConstantSingletonDecomposer.decompose(ExampleProductData.ExampleConstantSingleton) mustNot be (JObject(Nil))
    }
    
    "Deserialization of ConstantWithRealField succeeds even when information is missing" in {
      ExampleProductData.ExampleConstantWithRealField.isInstanceOf[data.fringe.ConstantWithRealField] must be (true)
    }
    "Serialization of ConstantWithRealField has non-zero information content" in {
      ExampleProductData.ExampleConstantWithRealField.serialize mustNot be (JObject(Nil))
    }
  
    
    "Deserialization of ProductWithUnionField succeeds even when information is missing" in {
      ExampleProductData.ExampleProductWithUnionField.isInstanceOf[data.fringe.ProductWithUnionField] must be (true)
    }
    "Serialization of ProductWithUnionField has non-zero information content" in {
      ExampleProductData.ExampleProductWithUnionField.serialize mustNot be (JObject(Nil))
    }
  
  }
  object ExampleMultitypeData {
    
  }
  class DataCoproductSerializationTest extends Runner(DataCoproductSerializationExamples) with JUnit
  object DataCoproductSerializationExamples extends Specification {
    
  }
  class DataConstantsSerializationTest extends Runner(DataConstantsSerializationExamples) with JUnit
  object DataConstantsSerializationExamples extends Specification {
    "Deserialization of constant ConstantBool succeeds" in {
      Constants.ConstantBool.serialize.deserialize[Boolean] must be (Constants.ConstantBool)
    }
    "Serialization of constant ConstantBool has non-zero information content" in {
      Constants.ConstantBool.serialize mustNot be (JObject(Nil))
    }
  
  }
}

