package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/BitSet.html BitSet]]
  *   - mutable
  */
final class BitSetSpec extends BaseSpec {

  "BitSet" should {

    "be initialized with some elements" in {
      val bitset = mutable.BitSet(0, 2, 4)
      bitset(0) shouldBe true
      bitset(1) shouldBe false
      bitset(2) shouldBe true
      bitset(3) shouldBe false
      bitset(4) shouldBe true
      bitset(5) shouldBe false
    }

    "be added an element using +=" in {
      val bitset = mutable.BitSet.empty
      bitset += 1
      bitset += 3
      bitset(0) shouldBe false
      bitset(1) shouldBe true
      bitset(2) shouldBe false
      bitset(3) shouldBe true
    }

    "be added elements using ++=" in {
      val bitset = mutable.BitSet.empty
      bitset ++= Seq(1, 2)
      bitset(0) shouldBe false
      bitset(1) shouldBe true
      bitset(2) shouldBe true
      bitset(3) shouldBe false
    }

    "be removed an element using -=" in {
      val bitset = mutable.BitSet(1, 2, 3)
      bitset(1) shouldBe true
      bitset(2) shouldBe true
      bitset -= 1
      bitset -= 2
      bitset(1) shouldBe false
      bitset(2) shouldBe false
      bitset(3) shouldBe true
    }

    "union between two bitsets" in {
      val s1 = mutable.BitSet(0, 1, 2)
      val s2 = mutable.BitSet(1, 2, 3)
      val s = s1 | s2
      s shouldBe mutable.BitSet(0, 1, 2, 3)
      s shouldBe (s1 union s2)
    }

    "intersection between two bitsets" in {
      val s1 = mutable.BitSet(0, 1, 2)
      val s2 = mutable.BitSet(1, 2, 3)
      val s = s1 & s2
      s shouldBe mutable.BitSet(1, 2)
      s shouldBe (s1 intersect s2)
    }

    "difference between two bitsets" in {
      val s1 = mutable.BitSet(0, 1, 2)
      val s2 = mutable.BitSet(1, 2, 3)
      val s = s1 &~ s2
      s shouldBe mutable.BitSet(0)
      s shouldBe (s1 diff s2)
    }

  }

}
