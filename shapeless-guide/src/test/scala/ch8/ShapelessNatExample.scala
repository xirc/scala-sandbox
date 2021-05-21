package ch8

import shapeless.ops.{coproduct, hlist, nat}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Nat, Succ}
import shapeless.ops.nat.ToInt
import testing.BaseSpec

import scala.annotation.nowarn

final class ShapelessNatExample extends BaseSpec {

  "representing numbers as types" in {

    type Zero = Nat._0
    type One = Succ[Zero]
    type Two = Succ[One]

    ToInt[Two].apply() shouldBe 2
    Nat.toInt[One] shouldBe 1

  }

  "length of generic representations" in {

    val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]
    Nat.toInt[hlistLength.Out] shouldBe 3

    val coproductLength = coproduct.Length[Double :+: Char :+: CNil]
    Nat.toInt[coproductLength.Out] shouldBe 2

    trait SizeOf[A] {
      def value: Int
    }
    def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

    implicit def genericSizeOf[A, L <: HList, N <: Nat](implicit
        @nowarn generic: Generic.Aux[A, L],
        @nowarn size: hlist.Length.Aux[L, N],
        sizeToInt: nat.ToInt[N]
    ): SizeOf[A] = new SizeOf[A] {
      override def value: Int = sizeToInt()
    }

    implicit def coproductSizeOf[A, L <: Coproduct, N <: Nat](implicit
        @nowarn generic: Generic.Aux[A, L],
        @nowarn size: coproduct.Length.Aux[L, N],
        sizeToInt: nat.ToInt[N]
    ): SizeOf[A] = new SizeOf[A] {
      override def value: Int = sizeToInt()
    }

    case class ClassA(name: String, value: Int)
    sizeOf[ClassA] shouldBe 2

    sealed trait Shape
    case class Circle(radius: Int) extends Shape
    case class Square(size: Int) extends Shape
    sizeOf[Shape] shouldBe 2

  }

}
