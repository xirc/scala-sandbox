package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/SortedSet.html SortedSet]]
  *  - mutable
  */
final class SortedSetSpec extends BaseSpec {

  "SortedSet" should {

    "be initialized with 3 elements" in {
      val s = mutable.SortedSet(1, 2, 3)
      s(1) shouldBe true
      s(2) shouldBe true
      s(3) shouldBe true
    }

    "be added an element using +=" in {
      val s = mutable.SortedSet(1, 2, 3)
      s(4) shouldBe false
      s += 4
      s(4) shouldBe true
    }

    "be added elements using ++=" in {
      val s = mutable.SortedSet(1, 2, 3)
      s(4) shouldBe false
      s(5) shouldBe false
      s ++= Seq(4, 5)
      s(4) shouldBe true
      s(5) shouldBe true
    }
  }

  "union between two sets" in {
    val s1 = mutable.SortedSet(1, 2, 3)
    val s2 = mutable.SortedSet(2, 3, 4)
    val s = s1 | s2
    s shouldBe mutable.Set(1, 2, 3, 4)
    s shouldBe (s1 union s2)
  }

  "intersection between two sets" in {
    val s1 = mutable.SortedSet(1, 2, 3)
    val s2 = mutable.SortedSet(2, 3, 4)
    val s = s1 & s2
    s shouldBe mutable.Set(2, 3)
    s shouldBe (s1 intersect s2)
  }

  "difference between two sets" in {
    val s1 = mutable.SortedSet(1, 2, 3)
    val s2 = mutable.SortedSet(2, 3, 4)
    val s = s1 &~ s2
    s shouldBe mutable.Set(1)
    s shouldBe (s1 diff s2)
  }

  "default SortedSet implementation is TreeSet" in {
    val s = mutable.SortedSet(1, 2, 3)
    s shouldBe a[mutable.TreeSet[_]]
  }

  "range" in {
    val s = mutable.SortedSet(1, 2, 3, 4, 5, 6)
    s.range(2, 5).toSeq shouldBe Seq(2, 3, 4)
  }
  
  "rangeFrom" in {
    val s = mutable.SortedSet(1, 2, 3, 4, 5, 6)
    s.rangeFrom(3).toSeq shouldBe Seq(3, 4, 5, 6)
  }
  
  "rangeUntil" in {
    val s = mutable.SortedSet(1, 2, 3, 4, 5, 6)
    s.rangeUntil(4).toSeq shouldBe Seq(1, 2, 3)
  }
  
  "rangeTo" in {
    val s = mutable.SortedSet(1, 2, 3, 4, 5, 6)
    s.rangeTo(4).toSeq shouldBe Seq(1, 2, 3, 4)
  }

}
