import io.circe.Json

@HalResource
case class Cost(id: Int, amount: Double) extends MdsolObject {
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
