package ch4

import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.Last
import testing.BaseSpec

object DependentlyTypedFunction {

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }
    def apply[L <: HList](implicit ev: Second[L]): Aux[L, ev.Out] = ev

    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        type Out = B
        override def apply(value: A :: B :: Rest): B =
          value.tail.head
      }
  }

}

final class DependentlyTypedFunction extends BaseSpec {
  import DependentlyTypedFunction._

  "Last" in {

    val last1 = Last[String :: Int :: HNil]
    val last2 = Last[Int :: String :: HNil]

    last1("foo" :: 1 :: HNil) shouldBe 1
    last2(1 :: "abc" :: HNil) shouldBe "abc"

  }

  "Second" in {

    val second1 = Second[String :: Int :: Boolean :: HNil]
    val second2 = Second[String :: Boolean :: Int :: HNil]

    second1("foo" :: 1 :: true :: HNil) shouldBe 1
    second2("baz" :: false :: 2 :: HNil) shouldBe false

  }

}
