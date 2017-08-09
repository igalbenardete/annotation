
@TestHalResource
case class TestCost(id: String, @TestHalEmbedded testInvoice: TestInvoice)

case class TestInvoice(id: String, amount: Double)