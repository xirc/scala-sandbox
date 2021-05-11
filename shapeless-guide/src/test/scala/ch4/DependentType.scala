package ch4

import shapeless.{Generic, HNil}
import testing.BaseSpec

final class DependentType extends BaseSpec {

  // the result type depends on its value parameter
  def repr[A](value: A)(implicit gen: Generic[A]): gen.Repr =
    gen.to(value)

  case class Point(x: Int, y: Int)
  repr(Point(1, 2)) shouldBe 1 :: 2 :: HNil

}
