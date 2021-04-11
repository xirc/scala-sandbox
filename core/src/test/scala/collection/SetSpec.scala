package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/Set.html Set]]
  *  - mutable
  */
final class SetSpec extends BaseSpec {

  "Set" should {

    "be initialized with 3 elements" in {
      val set = mutable.Set(1, 2, 3)
      set(1) shouldBe true
      set(2) shouldBe true
      set(3) shouldBe true
    }

    "be added elements using +=" in {
      val set = mutable.Set.empty[Int]
      set += 1
      set += 2
      set += 3
      set(1) shouldBe true
      set(2) shouldBe true
      set(3) shouldBe true
    }

    "be added elements using ++=" in {
      val set = mutable.Set.empty[Int]
      set ++= Seq(1, 2, 3)
      set(1) shouldBe true
      set(2) shouldBe true
      set(3) shouldBe true
    }

    "be removed elements using -=" in {
      val set = mutable.Set(1, 2, 3)
      set -= 2
      set(1) shouldBe true
      set(2) shouldBe false
      set(3) shouldBe true
    }

  }

  "union between two sets" in {
    val a = mutable.Set(1, 2, 3)
    val b = mutable.Set(2, 3, 4)
    val c = (a | b)
    c shouldBe Set(1, 2, 3, 4)
    c shouldBe (a union b)
  }

  "intersection between two sets" in {
    val a = mutable.Set(1, 2, 3)
    val b = mutable.Set(2, 3, 4)
    val c = (a & b)
    c shouldBe Set(2, 3)
    c shouldBe (a intersect b)
  }

  "difference between two sets" in {
    val a = mutable.Set(1, 2, 3)
    val b = mutable.Set(2, 3, 4)
    val c = (a &~ b)
    c shouldBe Set(1)
    c shouldBe (a diff b)
  }

  "default set implementation is HashSet" in {
    val s = mutable.Set(1, 2, 3)
    s shouldBe a[mutable.HashSet[_]]
  }

}
