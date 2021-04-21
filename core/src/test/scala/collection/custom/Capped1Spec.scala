package collection.custom

import testing.BaseSpec

private final class Capped1Spec extends BaseSpec {

  val c1 = new Capped1(capacity = 4)

  val c2 = c1 :+ 1 :+ 2 :+ 3
  c2 shouldBe a[Capped1[_]]
  c2.toSeq shouldBe Seq(1, 2, 3)
  c2.length shouldBe 3
  c2.lastOption shouldBe Option(3)

  val c3 = c2 :+ 4 :+ 5 :+ 6
  c3.toSeq shouldBe Seq(3, 4, 5, 6)

  val c4 = c3.take(3)
  c4 should not be a[Capped1[_]]
  c4 shouldBe Seq(3, 4, 5)

}
