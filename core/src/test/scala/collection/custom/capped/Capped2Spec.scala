package collection.custom.capped

import testing.BaseSpec

private final class Capped2Spec extends BaseSpec {

  object Capped2 extends Capped2Factory(capacity = 4)

  val c1 = Capped2(1, 2, 3)
  c1 shouldBe a[Capped2[_]]
  c1.toSeq shouldBe Seq(1, 2, 3)

  val c2 = c1.take(2)
  c2 shouldBe a[Capped2[_]]
  c2.toSeq shouldBe Seq(1, 2)

  val c3 = c1.filter(x => x % 2 == 1)
  c3 shouldBe a[Capped2[_]]
  c3.toSeq shouldBe Seq(1, 3)

  val c4 = c1.map(x => x * x)
  c4 shouldBe a[Capped2[_]]
  c4.toSeq shouldBe Seq(1, 4, 9)

  val c5 = List(1, 2, 3, 4, 5).to(Capped2)
  c5 shouldBe a[Capped2[_]]

}
