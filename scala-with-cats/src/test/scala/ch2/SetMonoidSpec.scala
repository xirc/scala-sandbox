package ch2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

object SetMonoidSpec {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit semigroup: Semigroup[A]): Semigroup[A] =
      semigroup
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
      monoid
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Semigroup[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

}

final class SetMonoidSpec extends AnyWordSpecLike with Matchers {
  import SetMonoidSpec._

  "union" in {

    implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty
      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

    Monoid[Set[Int]].combine(Set(1), Set(2)) shouldBe Set(1, 2)

    associativeLaw(Set(1), Set(2), Set(3)) shouldBe true
    identityLaw(Set(1)) shouldBe true

  }

  "intersect" in {

    implicit def intersectSemigroup[A]: Semigroup[Set[A]] =
      new Semigroup[Set[A]] {
        override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
      }

    Semigroup[Set[Int]].combine(Set(1, 2), Set(1, 3)) shouldBe Set(1)

    associativeLaw(Set(0, 1, 2), Set(0, 2, 3), Set(0, 1, 3)) shouldBe true

  }

  "symmetric difference" in {

    implicit def symmetricDifferenceMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        override def empty: Set[A] = Set.empty
        override def combine(x: Set[A], y: Set[A]): Set[A] =
          (x diff y) union (y diff x)
      }

    Monoid[Set[Int]].combine(Set(1, 3), Set(2, 3)) shouldBe Set(1, 2)

    associativeLaw(Set(0, 1), Set(0, 2), Set(0, 3)) shouldBe true
    identityLaw(Set(1)) shouldBe true

  }

}
