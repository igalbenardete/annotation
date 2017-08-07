import scala.meta._
import scala.collection.immutable.Seq
import _root_.io.circe.Json


val s =
  source"""
    @TestHalResource
    case class TestCost(id: String = "asd", @TestHalEmbedded testInvoice: TestInvoice, @TestHalEmbedded anotherInvoice: TestInvoice)
  """

def createEncoder(className: Type.Name, embedded: Seq[Term.Param], state: Seq[Term.Param]) = {
  val embeddedJson = embedded.foldLeft(Json.obj()) { (acc, curr) =>
    acc.deepMerge {
      Json.obj(curr.name.value -> Json.fromString(s"a.${curr.name.value}"))
    }
  }
  val embeddedParamNames = embedded.map(_.name.value)

//  val embeddedJson = embedded.foldLeft(Json.obj()) { (acc, curr) =>
//    acc.deepMerge {
//      Json.obj(curr.name.value -> Json.fromString(s"a.${curr.name.value}"))
//    }
//  }

  q"""
       import _root_.io.circe.Encoder

       implicit def encoder = new Encoder[$className] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._

        def apply(a: $className): Json = {

          Json.obj(
            "_embedded" -> Json.obj(
            ${embeddedParamNames.map{p => s"$p -> a.$p.asJson"}.mkString(").deepMerge(Json.obj(")}
            )
          )

//          Json.obj(
//            "_embedded" -> Json.obj(
//              "testInvoice" -> a.testInvoice.asJson
//            ).deepMerge(Json.obj(
//              "testInvoice2" -> a.testInvoice.asJson
//            )
//            )
//          )
       }
     }
   """
}

def createOutput(embeddedParamNames: Seq[String]) = {

}

val (cls, companion) = s.children match {
  case Seq(q"${cls: Defn.Class}; ${companion: Defn.Object}") => (cls, companion)
  case Seq(cls: Defn.Class) => (cls, q"object ${Term.Name(cls.name.value)}")
  case x =>
    println(x.structure)
    abort("@TestHalResource must annotate a class")
}
//cls.structure


val params = cls.ctor.paramss.flatten

val (paramsWithAnnotation, paramsWithoutAnnotation) = params.partition(_.mods.map(_.syntax).contains(mod"@TestHalEmbedded".syntax))

val newStats = createEncoder(Type.Name(cls.name.value), paramsWithAnnotation, paramsWithoutAnnotation) +: companion.templ.stats.getOrElse(Nil)

val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))

//val embeddedJson = paramsWithAnnotation.foldLeft(Json.obj()) { (acc, curr) =>
//  acc.deepMerge {
//    Json.obj(curr.name.value -> Json.fromString(s"a.${curr.name.value}"))
//  }
//}


//val embeddedParams = paramsWithAnnotation.foldLeft[Map[String, Term.Param]](Map.empty) { (acc, curr) =>
//  acc + (curr.name.value -> curr.copy(mods = Nil))
//}

////val p = Term.Param(Nil, Term.Name("_embedded"), Some(Type.Name("Map[String,String]")), None)
////val p = Term.Param(Nil, Term.Name("_embedded"), Some(Type.Name("Map[String,T]")),
////  Some(
////    Term.Apply(Term.Name("Map"), Seq(Term.ApplyInfix(Lit.String("key1"), Term.Name("->"), Nil, Seq(Lit.String("value1")))))
////  ))
//val p = Term.Param(Nil, Term.Name("_embedded"), Some(Type.Name("Map[String,scala.meta.Term.Param]")),
//  Some(
//    Term.Apply(Term.Name("Map"), {
//      val x = paramsWithAnnotation.map { param =>
//        Term.ApplyInfix(Lit.String(param.name.value), Term.Name("->"), Nil, Seq(Term.Apply(???, ???)))
//      }
//      ???
//      //      Seq(Term.ApplyInfix(Lit.String("key1"), Term.Name("->"), Nil, Seq(Lit.String("value1"))))
//    })
//  )
//)
//
////val p = Term.Param(
////  Nil,
////  Term.Name("_embedded"),
////  Some(Type.Name("Map[String,String]")),
////  Some(Term.Apply(Term.Name("Map"), Seq(Term.ApplyInfix(Lit.String("key1"), Term.Name("->"), Nil, Seq(Lit.String("value1"))), Term.ApplyInfix(Lit.String("key2"), Term.Name("->"), Nil, Seq(Lit.String("value2"))))))
////)
//
//val classParams = paramsWithoutAnnotation :+ p
//
//val className = Type.Name("Hal")
////val halResource = q"case class $className[..${cls.tparams :+ Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Type.Name("TestHalModel"))), Nil, Nil)}](..$classParams)"
//val halResource = q"case class $className[..${cls.tparams}](..$classParams)"
//
//
////
////p.syntax

Json.obj(
  "_embedded" -> Json.obj(
    "testInvoice" -> Json.fromString("test")
  ).deepMerge(Json.obj(
    "testInvoice2" -> Json.fromString("test2")
  )
  )
)