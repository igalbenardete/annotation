package com.mdsol.akkahttp.annotations

@TestHalResource
case class TestCost(id: String, @TestHalEmbedded testInvoice: TestInvoice)

case class TestInvoice(id: String)