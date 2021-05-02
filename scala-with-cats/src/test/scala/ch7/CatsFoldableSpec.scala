package ch7

import cats.{Eval, Foldable}
import cats.instances.lazyList._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsFoldableSpec extends AnyWordSpecLike with Matchers {

  "foldLeft with List" in {

    val xs = List(1, 2, 3)
    val sum = Foldable[List].foldLeft(xs, 0)(_ + _)
    sum shouldBe 6

  }

  "foldLeft with Option" in {

    val someX = Option(1)
    val y = Foldable[Option].foldLeft(someX, 100)(_ * _)
    y shouldBe 100

    val noneX: Option[Int] = None
    val z = Foldable[Option].foldLeft(noneX, 1)(_ * _)
    z shouldBe 1

  }

  "foldRight with LazyList" in {

    val bigData = (1 to 100_000).to(LazyList)
    val sum = Foldable[LazyList].foldRight(bigData, Eval.now(0L)) {
      (value, acc) =>
        acc.map(value + _)
    }
    val expectedSum = (1 + 100_000) * 100_000L / 2
    sum.value shouldBe expectedSum

  }

  "find" in {

    val xs = List(1, 2, 3)
    val ys = Foldable[List].find(xs)(_ % 2 == 0)
    ys shouldBe Option(2)

    val x = Option(1)
    Foldable[Option].find(x)(_ % 2 == 0) shouldBe None
    val y: Option[Int] = None
    Foldable[Option].find(y)(_ % 2 == 0) shouldBe None

  }

  "exists" in {

    val xs = List(1, 2, 3)
    val hasEven = Foldable[List].exists(xs)(_ % 2 == 0)
    hasEven shouldBe true

    val x = Option(2)
    Foldable[Option].exists(x)(_ % 2 == 0) shouldBe true
    val y: Option[Int] = None
    Foldable[Option].exists(y)(_ % 2 == 0) shouldBe false

  }

  "toList" in {

    val xs = Vector(1, 2, 3)
    val ys = Foldable[Vector].toList(xs)
    ys shouldBe List(1, 2, 3)

    val x = Option(1)
    Foldable[Option].toList(x) shouldBe List(1)
    val y = None
    Foldable[Option].toList(y) shouldBe List.empty

  }

  "isEmpty" in {

    val xs = List(1, 2, 3)
    Foldable[List].isEmpty(xs) shouldBe false

    val x = Option(1)
    Foldable[Option].isEmpty(x) shouldBe false
    val y = None
    Foldable[Option].isEmpty(y) shouldBe true

  }

  "nonEmpty" in {

    val xs = List(1, 2, 3)
    Foldable[List].nonEmpty(xs) shouldBe true

    val x = Option(1)
    Foldable[Option].nonEmpty(x) shouldBe true
    val y = None
    Foldable[Option].nonEmpty(y) shouldBe false

  }

  "combineAll" in {

    val xs = List(1, 2, 3)
    Foldable[List].combineAll(xs) shouldBe 6

    // use syntax
    xs.combineAll shouldBe 6

  }

  "foldMap" in {

    val xs = List(1, 2, 3)
    Foldable[List].foldMap(xs)(_.toString) shouldBe "123"

    // use syntax
    xs.foldMap(_.toString) shouldBe "123"

  }

  "compose" in {

    val xs = List(Vector(1, 2, 3), Vector(4, 5, 6))
    val foldable = (Foldable[List] compose Foldable[Vector])
    foldable.combineAll(xs) shouldBe 21

  }

}
