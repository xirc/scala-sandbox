package ch4

import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{IsHCons, Last}
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

  "chain" in {

    def lastField[A, Repr <: HList](input: A)(implicit
        gen: Generic.Aux[A, Repr],
        last: Last[Repr]
    ): last.Out = last.apply(gen.to(input))

    case class ClassA(x: Int)
    case class ClassB(x: Int, y: Double)
    case class ClassC(x: Int, y: Double, z: Boolean)
    lastField(ClassA(1)) shouldBe 1
    lastField(ClassB(1, 1.23)) shouldBe 1.23
    lastField(ClassC(1, 1.23, false)) shouldBe false

  }

  "chain2" in {

    def unwrap[A, Repr <: HList, H](input: A)(implicit
        gen: Generic.Aux[A, Repr],
        isHCons: IsHCons.Aux[Repr, H, HNil]
    ): H = gen.to(input).head

    case class ClassA(x: Int)

    unwrap(ClassA(1)) shouldBe 1

  }

}
