import scala.annotation.StaticAnnotation
import scala.meta._
import scala.collection.immutable.Seq


trait TestHalModel

class TestHalEmbedded extends StaticAnnotation

class TestHalResource extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val (cls, companion) = defn match {
      case q"${cls: Defn.Class}; ${companion: Defn.Object}" => (cls, companion)
      case cls: Defn.Class => (cls, q"object ${Term.Name(cls.name.value)}")
      case _ => abort("@TestHalResource must annotate a class")

    }

    val flatten = cls.ctor.paramss.flatten
    flatten.foreach(println(_))
    val (paramsWithAnnotation, paramsWithoutAnnotation) = flatten.partition(_.mods.map(_.syntax).contains(mod"@TestHalEmbedded".syntax))

    val className = Type.Name("Hal")


//    val mods = Seq.empty
//    val name = "aasdf"
//    val tpeopt = Type.Arg.ByName
//    val p = param"..$mods $name: $tpeopt"
//    Term.Param(Seq.empty[Mod], Term.Name("_embedded"), Some(Type.Arg)

//    paramsWithoutAnnotation.foldLeft(Map.empty[String, Type.Param]){ (acc, curr) =>
//      acc + (curr.)
//    }
    val classParams = paramsWithoutAnnotation
    val halResource = q"case class $className[..${cls.tparams}](..$classParams)"

    val newCompanion = companion.copy(
      templ = companion.templ.copy(
        stats = Some(companion.templ.stats.getOrElse(Nil) :+ halResource)
    ))
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
