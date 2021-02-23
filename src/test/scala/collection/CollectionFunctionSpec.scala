package collection

import testing.BaseSpec

final class CollectionFunctionSpec extends BaseSpec {

  "collect" in {
    val numbers = Vector.tabulate(1000)(i => i)
    val evenNumbers = numbers.collect {
      case n if n % 2 == 0 => n
    }
    val oddNumbers = numbers.collect {
      case n if n % 2 == 1 => n
    }
    evenNumbers.size shouldBe 500
    oddNumbers.size shouldBe 500
    evenNumbers.forall(_ % 2 == 0) shouldBe true
    oddNumbers.forall(_ % 2 == 1) shouldBe true
  }

  "drop" in {
    val numbers = Vector(0, 1, 2, 3, 4)
    numbers.drop(3) shouldBe Vector(3, 4)
  }

}
