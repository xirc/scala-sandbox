package ch6

import shapeless._
import shapeless.ops.hlist
import testing.BaseSpec

object CustomOpsExample {

  trait Penultimate[L] {
    type Out
    def apply(l: L): Out
  }

  object Penultimate {

    type Aux[L, O] = Penultimate[L] { type Out = O }

    def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

    implicit def hlistPenultimate[L <: HList, M <: HList, O](implicit
        init: hlist.Init.Aux[L, M],
        last: hlist.Last.Aux[M, O]
    ): Penultimate.Aux[L, O] =
      new Penultimate[L] {
        override type Out = O
        override def apply(l: L): O =
          last(init(l))
      }

    implicit def genericPenultimate[A, R, O](implicit
        generic: Generic.Aux[A, R],
        penultimate: Penultimate.Aux[R, O]
    ): Penultimate.Aux[A, O] =
      new Penultimate[A] {
        override type Out = O
        override def apply(l: A): O =
          penultimate(generic.to(l))
      }

  }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit p: Penultimate[A]): p.Out = p.apply(a)
  }

}

final class CustomOpsExample extends BaseSpec {
  import CustomOpsExample._

  "primary" in {

    type BigList = String :: Int :: Boolean :: Double :: HNil
    val xs: BigList = "abc" :: 123 :: true :: 4.56 :: HNil

    Penultimate[BigList].apply(xs) shouldBe true
    xs.penultimate shouldBe true

  }

  "product" in {

    final case class ClassA(key: String, value: Int)

    val instance = ClassA("abc", 123)

    Penultimate[ClassA].apply(instance) shouldBe "abc"
    instance.penultimate shouldBe "abc"

  }

}
