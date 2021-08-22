package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/TreeMap.html TreeMap]]
  *   - mutable
  *   - sorted
  */
final class TreeMapSpec extends BaseSpec {

  "TreeMap" should {

    "be initialized with 3 elements" in {
      val mp = mutable.TreeMap("a" -> 3, "b" -> 2, "c" -> 1)
      mp("a") shouldBe 3
      mp("b") shouldBe 2
      mp("c") shouldBe 1
    }

    "be Added an element using +=" in {
      val mp = mutable.TreeMap.empty[Int, String]
      mp += 1 -> "one"
      mp(1) shouldBe "one"
    }

    "be Added elements using ++=" in {
      val mp = mutable.TreeMap.empty[Int, String]
      mp ++= Seq(
        1 -> "one",
        2 -> "two"
      )
      mp(1) shouldBe "one"
      mp(2) shouldBe "two"
    }

    "be removed a key using -=" in {
      val mp = mutable.TreeMap(1 -> "one", 2 -> "two")
      mp -= 1
      mp.contains(1) shouldBe false
      mp(2) shouldBe "two"
    }

    "be iterated ordered by key" in {
      val mp = mutable.TreeMap("c" -> 1, "b" -> 3, "a" -> 2)
      mp.keys.toSeq shouldBe Seq("a", "b", "c")
    }

  }

}
