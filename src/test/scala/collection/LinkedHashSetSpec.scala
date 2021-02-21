package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/LinkedHashSet.html LinkedHashSet]]
  *  - mutable
  */
final class LinkedHashSetSpec extends BaseSpec {

  "LinkedHashSet" should {

    "be initialized with 3 elements" in {
      val s = mutable.LinkedHashSet(1, 2, 3)
      s(1) shouldBe true
      s(2) shouldBe true
      s(3) shouldBe true
    }

    "be added an element using +=" in {
      val s = mutable.LinkedHashSet.empty[Int]
      s(1) shouldBe false
      s(2) shouldBe false
      s += 1
      s += 2
      s(1) shouldBe true
      s(2) shouldBe true
    }

    "be added elements using ++=" in {
      val s = mutable.LinkedHashSet.empty[Int]
      s(1) shouldBe false
      s(2) shouldBe false
      s ++= Seq(1, 2)
      s(1) shouldBe true
      s(2) shouldBe true
    }

    "be removed an element using -=" in {
      val s = mutable.LinkedHashSet(1, 2)
      s(1) shouldBe true
      s(2) shouldBe true
      s -= 1
      s -= 2
      s(1) shouldBe false
      s(2) shouldBe false
    }

    "union between two sets" in {
      val s1 = mutable.LinkedHashSet(1, 2, 3)
      val s2 = mutable.LinkedHashSet(2, 3, 4)
      val s3 = s1 | s2
      s3 shouldBe mutable.LinkedHashSet(1, 2, 3, 4)
      s3 shouldBe (s1 union s2)
    }

    "intersection between two sets" in {
      val s1 = mutable.LinkedHashSet(1, 2, 3)
      val s2 = mutable.LinkedHashSet(2, 3, 4)
      val s3 = s1 & s2
      s3 shouldBe mutable.LinkedHashSet(2, 3)
      s3 shouldBe (s1 intersect s2)
    }

    "difference between two sets" in {
      val s1 = mutable.LinkedHashSet(1, 2, 3)
      val s2 = mutable.LinkedHashSet(2, 3, 4)
      val s3 = s1 &~ s2
      s3 shouldBe mutable.LinkedHashSet(1)
      s3 shouldBe (s1 diff s2)
    }

    "iterate elements in order inserted to the set" in {
      val s = mutable.LinkedHashSet("buy", "me", "coffee")
      s.toSeq shouldBe Seq("buy", "me", "coffee")

      s += "and"
      s ++= Seq(",", "tea")
      s.toSeq shouldBe Seq("buy", "me", "coffee", "and", ",", "tea")

      // No effect in order if the value already exists.
      s += "buy"
      s.toSeq shouldBe Seq("buy", "me", "coffee", "and", ",", "tea")
    }

  }

}
