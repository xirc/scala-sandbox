package ch2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

object TruthMonoidSpec {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
      monoid
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

}

final class TruthMonoidSpec extends AnyWordSpecLike with Matchers {
  import TruthMonoidSpec._

  "and" in {

    implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    Monoid[Boolean].combine(false, false) shouldBe false
    Monoid[Boolean].combine(false, true) shouldBe false
    Monoid[Boolean].combine(true, false) shouldBe false
    Monoid[Boolean].combine(true, true) shouldBe true

    associativeLaw(false, false, false) shouldBe true
    associativeLaw(true, false, false) shouldBe true
    associativeLaw(true, true, false) shouldBe true
    associativeLaw(true, true, true) shouldBe true

    identityLaw(true) shouldBe true
    identityLaw(false) shouldBe true

  }

  "or" in {

    implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

    Monoid[Boolean].combine(false, false) shouldBe false
    Monoid[Boolean].combine(false, true) shouldBe true
    Monoid[Boolean].combine(true, false) shouldBe true
    Monoid[Boolean].combine(true, true) shouldBe true

    associativeLaw(false, false, false) shouldBe true
    associativeLaw(true, false, false) shouldBe true
    associativeLaw(true, true, false) shouldBe true
    associativeLaw(true, true, true) shouldBe true

    identityLaw(true) shouldBe true
    identityLaw(false) shouldBe true

  }

  "xor" in {

    implicit val xorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean =
        (x && !y) || (!x && y)
    }

    Monoid[Boolean].combine(false, false) shouldBe false
    Monoid[Boolean].combine(false, true) shouldBe true
    Monoid[Boolean].combine(true, false) shouldBe true
    Monoid[Boolean].combine(true, true) shouldBe false

    associativeLaw(false, false, false) shouldBe true
    associativeLaw(true, false, false) shouldBe true
    associativeLaw(true, true, false) shouldBe true
    associativeLaw(true, true, true) shouldBe true

    identityLaw(true) shouldBe true
    identityLaw(false) shouldBe true

  }

  "xnor" in {

    implicit val xorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean =
        (x || !y) && (!x || y)
    }

    Monoid[Boolean].combine(false, false) shouldBe true
    Monoid[Boolean].combine(false, true) shouldBe false
    Monoid[Boolean].combine(true, false) shouldBe false
    Monoid[Boolean].combine(true, true) shouldBe true

    associativeLaw(false, false, false) shouldBe true
    associativeLaw(true, false, false) shouldBe true
    associativeLaw(true, true, false) shouldBe true
    associativeLaw(true, true, true) shouldBe true

    identityLaw(true) shouldBe true
    identityLaw(false) shouldBe true

  }

}
