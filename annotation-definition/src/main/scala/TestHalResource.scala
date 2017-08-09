import scala.annotation.StaticAnnotation
import scala.meta._
import scala.collection.immutable.Seq


trait TestHalModel

class TestHalEmbedded extends StaticAnnotation

class TestHalResource extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def createEncoder(className: Type.Name, embedded: Seq[Term.Param], state: Seq[Term.Param]) = {
      val embeddedParamNames = embedded.map(_.name.value)
      val innerSource = {
        val j =embeddedParamNames.map{p => s""""$p" -> a.$p.asJson"""}.mkString(").deepMerge(Json.obj(")
        if(embeddedParamNames.length > 1) j + ")" else j
      }
      val innerJson =
        s"""
           |val innerJson = Json.obj(
           |  ${innerSource}
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

    val (cls, companion) = defn match {
      case q"${cls: Defn.Class}; ${companion: Defn.Object}" => (cls, companion)
      case cls: Defn.Class => (cls, q"object ${Term.Name(cls.name.value)}")
      case _ => abort("@TestHalResource must annotate a class")

    }

    val params = cls.ctor.paramss.flatten
    val (paramsWithAnnotation, paramsWithoutAnnotation) = params.partition(_.mods.map(_.syntax).contains(mod"@TestHalEmbedded".syntax))

    val newStats = createEncoder(Type.Name(cls.name.value), paramsWithAnnotation, paramsWithoutAnnotation) +: companion.templ.stats.getOrElse(Nil)
    val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))

    //    val className = Type.Name("Hal")

    //    val classParams = paramsWithoutAnnotation
    //    val halResource = q"case class $className[..${cls.tparams}](..$classParams)"

    //    val newCompanion = companion.copy(
    //      templ = companion.templ.copy(
    //        stats = Some(companion.templ.stats.getOrElse(Nil) :+ halResource)
    //    ))
    //    val paramsWithAnnotation = for {
    //      param <- cls.ctor.paramss.flatten.filter(_.mods.contains(mod"@TestHalEmbedded"))
    //
    //      modifier <- param.mods.collect()
    //      newParam <- modifier match {
    //        case mod"@TestHalEmbedded" => ???
    //      }
    //    } yield (modifier)

    q"$cls; $newCompanion"
  }


}
