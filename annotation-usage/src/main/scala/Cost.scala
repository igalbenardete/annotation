import io.circe.Json


case class Cost(id: Int, amount: Double) extends MdsolObject() {
  def toHalJson: Json = {
    Json.fromString("".stripMargin)
  }
}

object Cost {
  {
    import _root_.io.circe.Encoder
    implicit def encoder = new Encoder[tName.type] {

      import _root_.io.circe.Json
      import _root_.io.circe.syntax._

      def apply(a: tName.type): Json = {
        val simpleJsonFields: Seq[(String, Json)] = parameters.map(field => field.name.syntax -> field.name.value.asJson)
        val baseSeq: Seq[(String, Json)] = Seq("_links" -> Json.obj("href" -> Json.obj("self" -> Json.fromString("self_reference")))) ++ simpleJsonFields
        val result: Seq[(String, Json)] = baseSeq ++ simpleJsonFields
        Json.fromFields(result)
      }
    }
  }
}
