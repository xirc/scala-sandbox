package ch2

import cats._
import cats.syntax.semigroup._
import testing.BaseSpec

final class CatsMonoid extends BaseSpec {

  "string monoid" in {

    Monoid[String].combine("Hi ", "there") shouldBe "Hi there"
    Monoid[String].empty shouldBe ""

  }

  "string semigroup" in {

    Semigroup[String].combine("Hi ", "there") shouldBe "Hi there"

  }

  "int monoid" in {

    Monoid[Int].combine(32, 10) shouldBe 42
    Monoid[Int].empty shouldBe 0

  }

  "option(int) monoid" in {

    Monoid[Option[Int]].combine(Some(22), Some(20)) shouldBe Some(42)
    Monoid[Option[Int]].empty shouldBe None

  }

  "combine syntax |+|" in {

    "Hi " |+| "there" shouldBe "Hi there"
    1 |+| 2 shouldBe 3
    Option(1) |+| Option(2) shouldBe Some(3)

  }

}
