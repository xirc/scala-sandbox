package ch3

import cats.Functor
import cats.instances.function._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import testing.BaseSpec

final class CatsFunctorSpec extends BaseSpec {

  "map" in {

    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    list2 shouldBe List(2, 4, 6)

    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)
    option2 shouldBe Option("123")

  }

  "lift" in {

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    liftedFunc(Option(1)) shouldBe Option(2)

  }

  "as" in {

    val list1 = List("A", "B", "C")
    val list2 = Functor[List].as(list1, "As")
    list2 shouldBe List("As", "As", "As")

  }

  "map syntax" in {

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => s"$a!"
    val func4 = func1.map(func2).map(func3)

    func4(123) shouldBe "248!"

  }

  "abstract over functors" in {

    def doMath[F[_]](start: F[Int])(implicit ev: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)

    doMath(Option(20)) shouldBe Option(22)
    doMath(List(1, 2, 3)) shouldBe List(3, 4, 5)

  }

  "as syntax" in {

    List(1, 2, 3).as("As") shouldBe List("As", "As", "As")

  }

}
