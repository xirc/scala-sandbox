package ch7

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class FoldSpec extends AnyWordSpecLike with Matchers {

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

}
