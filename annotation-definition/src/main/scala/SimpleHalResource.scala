import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
import _root_.io.circe.Json
import _root_.io.circe.syntax._

class SimpleHalResource extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"case class $tName (..$params) extends $template {..$stats}" = defn


    val encoderMethod = SimpleHalResourceImpl.quasi(tName, params)

    defn match {
      // companion object exists
      case Term.Block(
      Seq(cls@Defn.Class(_, name, _, ctor, _),
      companion: Defn.Object)) =>
        val templateStats =
          encoderMethod +: companion.templ.stats.getOrElse(Nil)
        val newCompanion = companion.copy(
          templ = companion.templ.copy(stats = Some(templateStats)))
        Term.Block(Seq(cls, newCompanion))
      // companion object does not exists
      case cls@Defn.Class(_, name, _, ctor, _) =>
        val companion = q"object ${Term.Name(name.value)} { $encoderMethod }"
        Term.Block(Seq(cls, companion))
      case _ =>
        println(defn.structure)
        abort("@WithApply must annotate a class.")
    }
  }
}

object SimpleHalResourceImpl {
  def halRepresentation(tName: Type.Name, parameters: Seq[Term.Param]): Json = {
    val simpleJsonFields: Seq[(String, Json)] = parameters.map(field => field.name.syntax -> field.name.value.asJson)
    val baseSeq: Seq[(String, Json)] = Seq(
      "_links" -> Json.obj(
        "href" -> Json.obj(
          "self" -> Json.fromString("self_reference")
        )
      ),
    ) ++ simpleJsonFields

    val result: Seq[(String, Json)] = baseSeq ++ simpleJsonFields
    Json.fromFields(result)
  }

  def quasi(tName: Type.Name, parameters: Seq[Term.Param]) = {

    val implicitEncoder =
      q"""
      import _root_.io.circe.Encoder

      implicit def encoder = new Encoder[tName.type] {

        import _root_.io.circe.Json
        import _root_.io.circe.syntax._

        def apply(a: tName.type): Json = {
          val simpleJsonFields: Seq[(String, Json)] = parameters.map(field => field.name.syntax -> field.name.value.asJson)

          val baseSeq: Seq[(String, Json)] = Seq(
            "_links" -> Json.obj(
              "href" -> Json.obj(
                "self" -> Json.fromString("self_reference")
              )
            ),
          ) ++ simpleJsonFields

          val result: Seq[(String, Json)] = baseSeq ++ simpleJsonFields
          Json.fromFields(result)
        }
      }
      """
    implicitEncoder
  }
}

/*
  private def createApply(className: Type.Name) = {
    q"""
       implicit def encoder = new Encoder[$className] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._
        import _root_.io.circe.Encoder

        def apply(a: $className): Json = {
          val (simpleFields: Seq[Term.Param], nonSimpleFields: Seq[Term.Param]) =
            params.partition(field => field.decltpe.fold(false) {
              case _: Type.Name => true
              case _ => false
            })

          val embedded: Seq[(String, Json)] = nonSimpleFields.map(field => field.name.syntax -> field.name.value.asJson)
          val simpleJsonFields: Seq[(String, Json)] = simpleFields.map(field => field.name.syntax -> field.name.value.asJson)

          val baseSeq: Seq[(String, Json)] = Seq(
            "_links" -> Json.obj(
              "href" -> Json.obj(
                "self" -> Json.fromString("self_reference")
              )
            ),
            "_embedded" -> Json.fromFields(embedded),
          ) ++ simpleJsonFields

          val result: Seq[(String, Json)] = baseSeq ++ simpleJsonFields
          Json.fromFields(result)
        }
       }
     """
  }

  private def halConversion(params: Seq[Term.Param]): Json = {
    val (simpleFields: Seq[Term.Param], nonSimpleFields: Seq[Term.Param]) =
      params.partition(field => field.decltpe.fold(false) {
        case _: Type.Name => true
        case _ => false
      })

    val embedded: Seq[(String, Json)] = nonSimpleFields.map(field => field.name.syntax -> field.name.value.asJson)
    val simpleJsonFields: Seq[(String, Json)] = simpleFields.map(field => field.name.syntax -> field.name.value.asJson)

    val baseSeq: Seq[(String, Json)] = Seq(
      "_links" -> Json.obj(
        "href" -> Json.obj(
          "self" -> Json.fromString("self_reference")
        )
      ),
      "_embedded" -> Json.fromFields(embedded),
    ) ++ simpleJsonFields

    val result: Seq[(String, Json)] = baseSeq ++ simpleJsonFields
    Json.fromFields(result)
  }

  private def isSimpleDataType(field: Term.Param): Boolean = {
    field.decltpe.fold(false) {
      case _: Type.Name => true
      case _ => false
    }
  }

*/

//class Class2Map extends scala.annotation.StaticAnnotation {
//  inline def apply(defn: Any): Any = meta {
//    defn match {
//      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
//        val namesToValues: immutable.Seq[Term.Tuple] = paramss.flatten.map { param =>
//          val a = param
//          val v1: String = param.name.syntax
//          val v2: Term.Name = Term.Name(param.name.value)
//
//          q"(${v1}, ${v2})"
//        }
//        val toMapImpl: Term = q"_root_.scala.collection.Map[String, Any](..$namesToValues)"
//        val toMap = q"def toMap: _root_.scala.collection.Map[String, Any] = $toMapImpl"
//        val templateStats = toMap +: template.stats.getOrElse(Nil)
//        cls.copy(templ = template.copy(stats = Some(templateStats)))
//      case _ =>
//        println(defn.structure)
//        abort("@Class2Map must annotate a class.")
//    }
//  }
//}