package ch2

import testing.BaseSpec

object MonoidBasicSpec {

  trait Monoid[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

}

final class MonoidBasicSpec extends BaseSpec {
  import MonoidBasicSpec._

  "Integer addition" in {

    implicit val intPlusMonoid: Monoid[Int] = new Monoid[Int] {
      override def combine(x: Int, y: Int): Int = x + y
      override def empty: Int = 0
    }

    // 2 + (1 + 4) == (2 + 1) + 4
    associativeLaw(2, 1, 4) shouldBe true
    // (2 + 0) == (0 + 2)
    identityLaw(2) shouldBe true

  }

  "String addition" in {

    implicit val stringPlusMonoid: Monoid[String] = new Monoid[String] {
      override def combine(x: String, y: String): String = x + y
      override def empty: String = ""
    }

    // "abc" + ("def" + "ghi") == ("abc" + "def") + "ghi"
    associativeLaw("abc", "def", "ghi") shouldBe true
    // ("abc" + "") == ("" + "abc")
    identityLaw("abc") shouldBe true

  }

  "Integer multiplication" in {

    implicit val intMulMonoid: Monoid[Int] = new Monoid[Int] {
      override def combine(x: Int, y: Int): Int = x * y
      override def empty: Int = 1
    }

    // 2 * (3 * 4) == (2 * 3) * 4
    associativeLaw(2, 3, 4) shouldBe true
    // (3 * 1) == (1 * 3)
    identityLaw(3) shouldBe true

  }

}
