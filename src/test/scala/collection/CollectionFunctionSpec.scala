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

  "foldRight" in {
    val numbers = Vector.tabulate(3)(_ + 1)
    val sum = numbers.foldRight(0)(_ + 2 * _)
    // (1 + 2 * (2 + 2 * 3))
    sum shouldBe 17
  }

  "foreach" in {
    val numbers = Vector.tabulate(3)(_ + 1)
    var sum = 0
    numbers.foreach {
      sum += _
    }
    sum shouldBe 6
  }

  "groupBy" in {
    val numbers = Vector.tabulate(10)(identity)
    val numbersByModulo = numbers.groupBy(_ % 3)
    numbersByModulo(0) shouldBe Vector(0, 3, 6, 9)
    numbersByModulo(1) shouldBe Vector(1, 4, 7)
    numbersByModulo(2) shouldBe Vector(2, 5, 8)
  }

  "head" in {
    val numbers = Vector(2, 3, 1)
    numbers.head shouldBe 2
    a[NoSuchElementException] shouldBe thrownBy {
      Vector.empty[Int].head
    }
  }

  "headOption" in {
    val numbers = Vector(2, 3, 1)
    numbers.headOption shouldBe Some(2)
    Vector.empty[Int].headOption shouldBe None
  }

  "isEmpty" in {
    val numbers = Vector(1, 2)
    numbers.isEmpty shouldBe false
    Vector.empty[Int].isEmpty shouldBe true
  }

  "last" in {
    val numbers = Vector(2, 3, 1)
    numbers.last shouldBe 1
    a[NoSuchElementException] shouldBe thrownBy {
      Vector.empty[Int].last
    }
  }

  "lastOption" in {
    val numbers = Vector(2, 3, 1)
    numbers.lastOption shouldBe Some(1)
    Vector.empty[Int].lastOption shouldBe None
  }

  "map" in {
    val numbers = Vector(2, 3, 1)
    val doubledNumbers = numbers.map(_ * 2)
    doubledNumbers shouldBe Vector(4, 6, 2)
  }

  "max" in {
    val numbers = Vector(3, 4, 2)
    numbers.max shouldBe 4
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].max
    }
  }

  "maxOption" in {
    val numbers = Vector(3, 4, 2)
    numbers.maxOption shouldBe Some(4)
    Vector.empty[Int].maxOption shouldBe None
  }

}
