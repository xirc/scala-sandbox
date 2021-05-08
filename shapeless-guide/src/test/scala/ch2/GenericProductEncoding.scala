package ch2

import shapeless.{::, Generic, HNil, Inl, Inr}
import testing.BaseSpec

final class GenericProductEncoding extends BaseSpec {

  "product" in {

    val product: String :: Int :: Boolean :: HNil =
      "Sunday" :: 1 :: false :: HNil

    product.head shouldBe "Sunday"
    product.tail.head shouldBe 1
    product.last shouldBe false

    val newProduct: Long :: String :: Int :: Boolean :: HNil =
      10L :: product

    newProduct.head shouldBe 10L
    newProduct.tail shouldBe product

    case class ClassA(name: String, amount: Int)
    case class ClassB(key: String, value: Int)

    val valueA: ClassA = ClassA("candy", 1)
    val repr: String :: Int :: HNil = Generic[ClassA].to(valueA)
    repr shouldBe ("candy" :: 1 :: HNil)
    Generic[ClassA].from(repr) shouldBe valueA

    val valueB: ClassB = Generic[ClassB].from(repr)
    valueB shouldBe ClassB("candy", 1)

  }

  "coproduct" in {

    sealed trait Shape
    final case class Circle(radius: Double) extends Shape
    final case class Square(size: Int) extends Shape

    val gen = Generic[Shape]
    gen.to(Circle(2)) shouldBe Inl(Circle(2))
    gen.to(Square(3)) shouldBe Inr(Inl(Square(3)))

  }

}
