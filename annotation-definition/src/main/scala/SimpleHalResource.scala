import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
import scala.reflect.runtime._
import _root_.io.circe.Json
import _root_.io.circe.syntax._

class SimpleHalResource extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val encoderMethod = ???

    defn match {
      case Term.Block(
      Seq(cls@Defn.Class(mods, name, typeParameters, Ctor.Primary(primaryMods, ctorName, primaryParameters), template), companion: Defn.Object)) =>
        val caseClassType: String = name.value
        val p: Seq[Term.Param] = primaryParameters.flatten
        val fieldNames: Seq[String] = primaryParameters.flatMap(_.map(termpParameter => termpParameter.name.value))

        val templateStats = encoderMethod +: companion.templ.stats.getOrElse(Nil)
        val newCompanion = companion.copy(templ = companion.templ.copy(stats = Some(templateStats)))
        Term.Block(Seq(cls, newCompanion))

      case cls@Defn.Class(mods, name, typeParameters, Ctor.Primary(primaryMods, ctorName, primaryParameters), template)=>
        val companion = q"object ${Term.Name(name.value)} { $encoderMethod }"
        Term.Block(Seq(cls, companion))
      case _ =>
        println(defn.structure)
        abort("@WithApply must annotate a class.")
    }

    ???
  }
}

object SimpleHalResourceImpl {

}


private def createEncoder(className: Type.Name) = {
  q"""
       import _root_.io.circe.Encoder

       implicit def encoder = new Encoder[${className.value}] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._

        def apply(a: ${className.value}): Json = {
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

private def simpleHalConversion(params: Seq[Term.Param], fieldValues: Seq[Any]): Json = {
  val zippedFields: Seq[(Term.Param, Any)] = params zip fieldValues
  val embedded = zippedFields.map(zippedField => zippedField._1.name.value -> zippedField._2.asJson)
  val baseSeq: Seq[(String, Json)] = Seq(
    "_links" -> Json.obj(
      "href" -> Json.obj(
        "self" -> Json.fromString("self_reference")
      )
    ),
    "_embedded" -> Json.fromFields(embedded),
  )
  Json.fromFields(baseSeq)
}

private def parseAsJsonFromType(parameters: Seq[(Term.Param, Any)]): Seq[(String, Json)] = {
  parameters.map {
    case (p: Term.Param, t:Some[Int]) => p.name.value -> t.get.asJson
    case (p: Term.Param, t: Some[Double]) => p.name.value -> t.get.asJson
    case (p: Term.Param, t: Some[String]) => p.name.value -> t.get.asJson
    // Not sure about defualt
  }
}


private def halConversion(params: Seq[Term.Param]): Json = {
  val (simpleFields: Seq[Term.Param], nonSimpleFields: Seq[Term.Param]) =
    params.partition(field => field.decltpe.fold(false) {
      case _: Type.Name => true
      case _ => false
    })

  val x = nonSimpleFields.head.name.syntax
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