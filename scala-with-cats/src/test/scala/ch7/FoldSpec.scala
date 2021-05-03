package ch7

import testing.BaseSpec

final class FoldSpec extends BaseSpec {

  "foldLeft" in {

    val xs = List(1, 2, 3)
    val ys = xs.foldLeft(List.empty[Int])((acc, value) => value :: acc)
    ys shouldBe List(3, 2, 1)

  }

  "foldRight" in {

    val xs = List(1, 2, 3)
    val ys = xs.foldRight(List.empty[Int])((value, acc) => value :: acc)
    ys shouldBe List(1, 2, 3)

  }

  "map" in {

    def map[A, B](xs: List[A])(f: A => B): List[B] = {
      xs.foldRight(List.empty[B]) { (value, acc) =>
        f(value) :: acc
      }
    }

    val xs = List(1, 2, 3)
    val ys = map(xs)(a => (a * 2).toString)
    ys shouldBe List("2", "4", "6")

  }

  "flatMap" in {

    def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
      xs.foldRight(List.empty[B]) { (value, acc) =>
        f(value) ::: acc
      }
    }

    val xs = List(1, 2, 3)
    val ys = flatMap(xs)(a => List(a.toString, (a * 2).toString))
    ys shouldBe List("1", "2", "2", "4", "3", "6")

  }

  "filter" in {

    def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
      xs.foldRight(List.empty[A]) { (value, acc) =>
        if (f(value)) value :: acc
        else acc
      }
    }

    val xs = List(1, 2, 3)
    val ys = filter(xs)(_ <= 2)
    ys shouldBe List(1, 2)

  }

  "sum" in {

    def sum[A: Numeric](xs: List[A]): A = {
      val ev = implicitly[Numeric[A]]
      xs.foldRight(ev.zero) { (value, acc) =>
        ev.plus(value, acc)
      }
    }

    val xs = List(1, 2, 3)
    sum(xs) shouldBe 6

  }

}
