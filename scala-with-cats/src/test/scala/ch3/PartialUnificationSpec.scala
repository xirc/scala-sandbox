package ch3

import cats.instances.function._
import cats.syntax.functor._
import cats.syntax.contravariant._
import testing.BaseSpec

final class PartialUnificationSpec extends BaseSpec {

  val func1: Int => Double = _.toDouble
  val func2: Double => Double = _ * 2
  val func3: Int => Double = func1.map(func2)

  func3(1) shouldBe 2

  val func3a: Int => Double = a => func2(func1(a))
  val func3b: Int => Double = func2.compose(func1)

  func3a(1) shouldBe 2
  func3b(1) shouldBe 2

  type <=[B, A] = A => B
  val func2b: Double <= Double = func2
  val func3c: Int => Double = func2b.contramap(func1)
  func3c(1) shouldBe 2

}
