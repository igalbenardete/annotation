
import _root_.io.circe.Json
import _root_.io.circe.syntax._

trait MyTrait

case class MyClass(id: Int, message: String) extends MyTrait

// val fieldNames = this.getClass.getDeclaredFields.map(_.getName)

def simpleHalConversion(params: Seq[String], fieldValues: Seq[Any]): Json = {
  val zippedFields = params zip fieldValues
  val embedded = zippedFields.map(zippedField => zippedField._1 -> zippedField._2.asJson)
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


val myObject = MyClass(1, "Hello")
val fieldNames: Seq[String] = myObject.getClass.getDeclaredFields.map(_.toString).toSeq
val fieldValues: Seq[Any] = myObject.productIterator.toSeq

val myJson = simpleHalConversion(fieldNames, fieldValues)