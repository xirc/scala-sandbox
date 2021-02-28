package collection

import testing.BaseSpec

final class CollectionFunctionSpec extends BaseSpec {

  "collect" in {
    val numbers = Vector.tabulate(1000)(identity)
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

  "dropWhile" in {
    val numbers = Vector.tabulate(10)(identity)
    numbers.dropWhile(_ < 7) shouldBe Vector(7, 8, 9)
  }

  "exists" in {
    val numbers = Vector(0, 1, 2, 3, 4)
    numbers.exists(_ > 1) shouldBe true
    numbers.exists(_ < 0) shouldBe false
  }

  "filter" in {
    val numbers = Vector(0, 1, 2, 3, 4, 5)
    numbers.filter(_ % 2 == 0) shouldBe Vector(0, 2, 4)
    numbers.filter(_ % 2 == 1) shouldBe Vector(1, 3, 5)
  }

  "filterNot" in {
    val numbers = Vector(0, 1, 2, 3, 4, 5)
    numbers.filterNot(_ % 2 == 0) shouldBe Vector(1, 3, 5)
    numbers.filterNot(_ % 2 == 1) shouldBe Vector(0, 2, 4)
  }

  "find" in {
    val numbers = Vector(0, 1, 2, 3, 4, 5)
    numbers.find(_ > 1) shouldBe Some(2)
    numbers.find(_ < 0) shouldBe None
  }

  "findLast" in {
    val numbers = Vector(0, 1, 2, 3, 4, 5)
    numbers.findLast(_ > 1) shouldBe Some(5)
    numbers.findLast(_ < 0) shouldBe None
  }

  "flatMap" in {
    val numbers = Vector(0, 1, 2, 3)
    val newNumbers = numbers.flatMap(Array.tabulate(_)(identity))
    newNumbers shouldBe Vector(0, 0, 1, 0, 1, 2)
  }

  "flatten" in {
    val numbers = Vector.tabulate(3, 3) { case (i, j) => i * j }
    val newNumbers = numbers.flatten
    newNumbers shouldBe Vector(0, 0, 0, 0, 1, 2, 0, 2, 4)
  }

  "fold" in {
    val numbers = Vector.tabulate(3)(_ + 1)
    val sum = numbers.fold(0)(_ * 2 + _)
    // ((0 * 2 + 1) * 2 + 2) * 2 + 3
    sum shouldBe 11
  }

  "foldLeft" in {
    val numbers = Vector.tabulate(3)(_ + 1)
    val sum = numbers.foldLeft(0)(_ * 2 + _)
    // ((0 * 2 + 1) * 2 + 2) * 2 + 3
    sum shouldBe 11
  }

}
