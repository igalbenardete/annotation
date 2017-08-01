import io.circe.{Encoder, Json}
import io.circe.syntax._

case class Cost(id: Int,
                amount: Double) extends MdsolObject {

  def toHalJson: Json = {
    Json.fromString(
      s"""
         |{
         |  "_links": {
         |      "self": {"href": "self_reference_link"}
         |   },
         |  "id": ${this.id},
         |  "amount": ${this.amount}
         |}
       """.stripMargin
    )
  }
}

//object Cost {
//  implicit val encodeCost: Encoder[Cost] = new Encoder[Cost] {
//    final def apply(a: Cost): Json = {
//      val fieldNames = a.getClass.getDeclaredFields.map(_.getName)
//      val fieldValues: Seq[Any] = a.productIterator.toSeq
//
//      val zipped: Array[(String, Any)] = fieldNames zip fieldValues
//      val jsonFields: Array[(String, Json)] = zipped.map(pair => pair._1 -> pair._2.asJson)
//
//      val baseSeq: Seq[(String, Json)] = Seq(
//        "_links" -> Json.obj(
//          "href" -> Json.obj(
//            "self" -> Json.fromString("self_reference")
//          )
//        ),
//        "_embedded" -> Json.fromFields(jsonFields),
//      )
//      ???
//    }
//  }
//}

