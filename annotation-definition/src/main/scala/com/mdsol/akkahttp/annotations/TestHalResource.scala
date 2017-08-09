package com.mdsol.akkahttp.annotations

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class TestHalResource extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def createEncoder(className: Type.Name, embedded: Seq[Term.Param], state: Seq[Term.Param]) = {
      val implicitParams = {
        if (embedded.isEmpty){
          ""
        } else {
          val embeddedParamTypes = embedded.flatMap(_.decltpe)
            .foldLeft[Set[String]](Set.empty) { (acc, curr) => acc + curr.toString() }

          val implicits = embeddedParamTypes.map(t => s"enc$t: Encoder[$t]")
          "(implicit " + implicits.mkString(",") + ")"
        }
      }
      val implicitEnc = s"implicit def encoder$implicitParams = enc".parse[Stat].get

      val innerSource = {
        val embeddedParamNames = embedded.map(_.name.value)
        val j = embeddedParamNames.map { p => s""""$p" -> a.$p.asJson""" }.mkString(").deepMerge(Json.obj(")
        if (embeddedParamNames.length > 1) j + ")" else j
      }
      val innerJson =
        s"""
           |val innerJson = Json.obj(
           |  ${innerSource}
           |)
   """.stripMargin.parse[Stat].get

      q"""
       import _root_.io.circe.Encoder

       val enc = new Encoder[$className] {
        import _root_.io.circe.Json
        import _root_.io.circe.syntax._
        import _root_.io.circe.generic.auto._

        def apply(a: $className): Json = {
          $innerJson
          Json.obj(
            "_embedded" -> innerJson
          )
       }
       $implicitEnc
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

    q"$cls; $newCompanion"
  }


}
