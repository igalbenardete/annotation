import io.circe.Json

class Invoice(id: Int,
              status: String,
              cost: Cost) extends MdsolObject {
  def toHalJson: Json = {
    Json.fromString(
      s"""
         |{
         |  "_links": {
         |     "self": {"href": "self_reference_link"}
         |   }
         |  "_embedded": {
         |     "cost": ${this.cost.toHalJson}
         |  },
         |  "id": ${this.id},
         |  "status": ${this.status}
         |}
        """.stripMargin
    )
  }
}
