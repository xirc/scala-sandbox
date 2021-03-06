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

  "init" in {
    val xs = Vector(1, 3, 2)
    xs.init shouldBe Vector(1, 3)
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

  "maxBy" in {
    val words = Vector("abc", "hello", "goodbye")
    words.maxBy(_.length) shouldBe "goodbye"
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[String].maxBy(_.length)
    }
  }

  "maxByOption" in {
    val words = Vector("abc", "hello", "goodbye")
    words.maxByOption(_.length) shouldBe Some("goodbye")
    Vector.empty[String].maxByOption(_.length) shouldBe None
  }

  "min" in {
    val numbers = Vector(3, 4, 2)
    numbers.min shouldBe 2
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].min
    }
  }

  "minOption" in {
    val numbers = Vector(3, 4, 2)
    numbers.minOption shouldBe Some(2)
    Vector.empty[Int].minOption shouldBe None
  }

  "minBy" in {
    val words = Vector("abc", "hello", "goodbye")
    words.minBy(_.length) shouldBe "abc"
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[String].minBy(_.length)
    }
  }

  "minByOption" in {
    val words = Vector("abc", "hello", "goodbye")
    words.minByOption(_.length) shouldBe Some("abc")
    Vector.empty[String].minByOption(_.length) shouldBe None
  }

  "mkString" in {
    val numbers = Vector(3, 4, 2)
    numbers.mkString shouldBe "342"
    numbers.mkString(",") shouldBe "3,4,2"
    numbers.mkString("[", ",", "]") shouldBe "[3,4,2]"
  }

  "nonEmpty" in {
    val numbers = Vector(1, 2)
    numbers.nonEmpty shouldBe true
    Vector.empty[Int].nonEmpty shouldBe false
  }

  "partition" in {
    val numbers = Vector.tabulate(10)(identity)
    val (even, odd) = numbers.partition(_ % 2 == 0)
    even shouldBe Vector(0, 2, 4, 6, 8)
    odd shouldBe Vector(1, 3, 5, 7, 9)
  }

  "reduce" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduce(_ * 2 + _)
    value shouldBe 11
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].reduce(_ * 2 + _)
    }
  }

  "reduceOption" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduceOption(_ * 2 + _)
    value shouldBe Some(11)
    Vector.empty[Int].reduceOption(_ * 2 + _) shouldBe None
  }

  "reduceLeft" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduceLeft(_ * 2 + _)
    value shouldBe 11
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].reduceLeft(_ * 2 + _)
    }
  }

  "reduceLeftOption" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduceLeftOption(_ * 2 + _)
    value shouldBe Some(11)
    Vector.empty[Int].reduceLeftOption(_ * 2 + _) shouldBe None
  }

  "reduceRight" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduceRight(_ + _ * 2)
    value shouldBe 17
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].reduceRight(_ + _ * 2)
    }
  }

  "reduceRightOption" in {
    val numbers = Vector(1, 2, 3)
    val value = numbers.reduceRightOption(_ + _ * 2)
    value shouldBe Some(17)
    Vector.empty[Int].reduceRightOption(_ + _ * 2) shouldBe None
  }

  "reverse" in {
    val numbers = Vector(1, 2, 3)
    numbers.reverse shouldBe Vector(3, 2, 1)
  }

  "reverseIterator" in {
    val xs = for (i <- Vector(1, 2, 3).reverseIterator) yield i * 2
    xs.toVector shouldBe Vector(6, 4, 2)
  }

  "scan" in {
    val xs = Vector.tabulate(5)(_ + 1).scan(0)(_ + _)
    xs shouldBe Vector(0, 1, 3, 6, 10, 15)
  }

  "scanLeft" in {
    val xs = Vector.tabulate(5)(_ + 1).scanLeft(0)(_ + _)
    xs shouldBe Vector(0, 1, 3, 6, 10, 15)
  }

  "scanRight" in {
    val xs = Vector.tabulate(5)(_ + 1).scanRight(0)(_ + _)
    xs shouldBe Vector(15, 14, 12, 9, 5, 0)
  }

  "size" in {
    Vector(1, 2, 3, 4).size shouldBe 4
    Vector.empty[Int].size shouldBe 0
  }

  "slice" in {
    Vector(1, 2, 3, 4).slice(1, 3) shouldBe Vector(2, 3)
    Vector(1, 2, 3, 4).slice(1, 5) shouldBe Vector(2, 3, 4)
  }

  "sortBy" in {
    case class Item(name: String, price: Int)
    val xs = Vector(Item("a", 2), Item("b", 3), Item("c", 1))
    xs.sortBy(_.price) shouldBe Vector(Item("c", 1), Item("a", 2), Item("b", 3))
    xs.sortBy(_.name) shouldBe Vector(Item("a", 2), Item("b", 3), Item("c", 1))
  }

  "sorted" in {
    val xs = Vector(3, 1, 2, 4)
    xs.sorted shouldBe Vector(1, 2, 3, 4)
  }

  "sortWith" in {
    case class Item(name: String, price: Int)
    val xs = Vector(Item("a", 2), Item("b", 3), Item("c", 1))
    xs.sortWith(_.price < _.price) shouldBe Vector(
      Item("c", 1),
      Item("a", 2),
      Item("b", 3)
    )
  }

  "tail" in {
    val xs = Vector(2, 3, 1)
    xs.tail shouldBe Vector(3, 1)
    a[UnsupportedOperationException] shouldBe thrownBy {
      Vector.empty[Int].tail
    }
  }

  "take" in {
    val xs = Vector.tabulate(5)(identity)
    xs.take(3) shouldBe Vector(0, 1, 2)
    Vector.empty[Int].take(2) shouldBe Vector.empty
  }

  "takeRight" in {
    val xs = Vector.tabulate(10)(identity)
    xs.takeRight(3) shouldBe Vector(7, 8, 9)
    Vector.empty[Int].takeRight(2) shouldBe Vector.empty
  }

  "takeWhile" in {
    val xs = Vector(1, 2, 4, 3, 5)
    xs.takeWhile(_ < 3) shouldBe Vector(1, 2)
  }

  "transpose" in {
    val xs = Vector(1, 2, 3)
    val ys = Vector(4, 5, 6)
    Vector(xs, ys).transpose shouldBe Vector(
      Vector(1, 4),
      Vector(2, 5),
      Vector(3, 6)
    )
  }

  "unzip" in {
    val vs = Vector(1 -> "one", 2 -> "two", 3 -> "three")
    val expected = (Vector(1, 2, 3), Vector("one", "two", "three"))
    vs.unzip shouldBe expected
  }

  "view" in {
    val numbers =
      (1 to 1_000_000_000).view
        .filter(_ % 2 == 0)
        .dropWhile(_ < 100)
        .take(5)
        .toVector
    numbers shouldBe Vector(100, 102, 104, 106, 108)
  }

  "withFilter" in {
    val numbers = (1 to 1_000).withFilter(_ % 2 == 0).map(_ / 2)
    numbers shouldBe Vector.tabulate(500)(_ + 1)
  }

  "zip" in {
    val xs = Vector(1, 2, 3)
    val ys = Vector("one", "two", "three")
    xs.zip(ys) shouldBe Vector(1 -> "one", 2 -> "two", 3 -> "three")
  }

  "zipWithIndex" in {
    val xs = Vector("zero", "one", "two", "three")
    xs.zipWithIndex shouldBe Vector(
      "zero" -> 0,
      "one" -> 1,
      "two" -> 2,
      "three" -> 3
    )
  }

}
