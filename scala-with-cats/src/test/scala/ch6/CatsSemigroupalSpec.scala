package ch6

import cats.Semigroupal
import cats.syntax.apply._
import cats.instances.option._
import cats.instances.function._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsSemigroupalSpec extends AnyWordSpecLike with Matchers {

  "option" in {

    Semigroupal[Option].product(
      Option(1),
      Option("a")
    ) shouldBe Option((1, "a"))

    Semigroupal[Option].product(
      Option(1),
      None
    ) shouldBe None

    Semigroupal.tuple3(
      Option(1),
      Option("a"),
      Option(true)
    ) shouldBe Option((1, "a", true))

    Semigroupal.tuple3(
      Option(1),
      Option("a"),
      None
    ) shouldBe None

    Semigroupal.map2(
      Option(1),
      Option(2)
    )(_ + _) shouldBe Option(3)

  }

  "function" in {

    val f1: Int => Double = _.toDouble * 2
    val f2: Int => Double = _.toDouble * 3
    val f3: Int => Double = Semigroupal.map2(f1, f2)(_ + _)
    f3(1) shouldBe 5.0

  }

  "tupled" in {

    (
      Option(1),
      Option("a")
    ).tupled shouldBe Option((1, "a"))

    (
      Option(1),
      Option("a"),
      Option(false)
    ).tupled shouldBe Option((1, "a", false))

    (
      Option(1),
      None
    ).tupled shouldBe None

  }

  "mapN" in {

    (
      Option(1),
      Option(2),
      Option(3)
    ).mapN(_ + _ + _) shouldBe Option(6)

  }

}
