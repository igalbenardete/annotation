import scala.meta._
import scala.collection.immutable.Seq
import _root_.io.circe.Json
val s =
  source"""
    @TestHalResource
    case class TestCost(id: String = "asd", @TestHalEmbedded testInvoice: TestInvoice, @TestHalEmbedded anotherInvoice: TestInvoice)
  """
def createEncoder(className: Type.Name, embedded: Seq[Term.Param], state: Seq[Term.Param]) = {
  val embeddedParamNames = embedded.map(_.name.value)
  val innerJson =
    s"""
       |val innerJson = Json.obj(
       |  ${embeddedParamNames.map{p => s""""$p" -> a.$p.asJson"""}.mkString(").deepMerge(Json.obj(").concat(")")}
       |)
   """.stripMargin.parse[Stat].get

  q"""
       import _root_.io.circe.Encoder

       implicit def encoder = new Encoder[$className] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._

        def apply(a: $className): Json = {
          $innerJson
          Json.obj(
            "_embedded" -> innerJson
          )
       }
     }
   """
}
val (cls, companion) = s.children match {
  case Seq(q"${cls: Defn.Class}; ${companion: Defn.Object}") => (cls, companion)
  case Seq(cls: Defn.Class) => (cls, q"object ${Term.Name(cls.name.value)}")
  case x =>
    println(x.structure)
    abort("@TestHalResource must annotate a class")
}
val params = cls.ctor.paramss.flatten
val (paramsWithAnnotation, paramsWithoutAnnotation) = params.partition(_.mods.map(_.syntax).contains(mod"@TestHalEmbedded".syntax))
val newStats = createEncoder(Type.Name(cls.name.value), paramsWithAnnotation, paramsWithoutAnnotation) +: companion.templ.stats.getOrElse(Nil)
val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))

val embeddedParamNames = paramsWithAnnotation.map(_.name.value)
//val innerSource = embeddedParamNames.map{p => s""""$p" -> a.$p.asJson"""}.mkString(").deepMerge(Json.obj(").concat(")")
val innerJson =
  s"""
     |val innerJson = Json.obj(
     |  ${embeddedParamNames.map{p => s""""$p" -> a.$p.asJson"""}.mkString(").deepMerge(Json.obj(").concat(")")}
     |)
   """.stripMargin.parse[Stat].get


val qqq = q"""
       import _root_.io.circe.Encoder

       implicit def encoder = new Encoder[Pippo] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._

        def apply(a: Pippo): Json = {
          ${innerJson}
          Json.obj(
            "_embedded" -> innerJson
          )
       }
     }
   """