package ch5

import testing.BaseSpec

final class LiteralType extends BaseSpec {

  var int1: 1 = 1
  assertDoesNotCompile("""
     |int1 = 2
     |""".stripMargin)

  var bTrue: true = true
  assertDoesNotCompile("""
      |bTrue = false
      |""".stripMargin)

  var hello: "hello" = "hello"
  assertDoesNotCompile(
    """
      |hello = "world"
      |""".stripMargin
  )

  (int1 + 1) shouldBe 2
  (!bTrue) shouldBe false
  (hello + " world") shouldBe "hello world"

}
