package ch3

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}
import testing.BaseSpec

object DerivingInstances {

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {

    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](f: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        override def encode(value: A): List[String] = f(value)
      }

    implicit val booleanEncoder: CsvEncoder[Boolean] =
      instance(b => if (b) List("yes") else List("no"))

    implicit val stringEncoder: CsvEncoder[String] =
      instance(s => List(s))

    implicit val intEncoder: CsvEncoder[Int] =
      instance(i => List(i.toString))

    implicit val doubleEncoder: CsvEncoder[Double] =
      instance(d => List(d.toString))

    implicit val hnilEncoder: CsvEncoder[HNil] =
      instance(_ => Nil)

    implicit def hlistEncoder[H, T <: HList](implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] =
      instance { case h :: t =>
        hEncoder.encode(h) ::: tEncoder.encode(t)
      }

    implicit def genericEncoder[A, R](implicit
        gen: Generic.Aux[A, R],
        enc: CsvEncoder[R]
    ): CsvEncoder[A] =
      instance(a => enc.encode(gen.to(a)))

    implicit val cnilEncoder: CsvEncoder[CNil] =
      instance(_ => throw new Exception())

    implicit def coproductEncoder[H, T <: Coproduct](implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] =
      instance {
        case Inl(h) => hEncoder.encode(h)
        case Inr(t) => tEncoder.encode(t)
      }

  }

  implicit class CsvEncoderOps[V](val value: V) extends AnyVal {
    def encoded(implicit enc: CsvEncoder[V]): List[String] =
      enc.encode(value)
  }

}

final class DerivingInstances extends BaseSpec {
  import DerivingInstances._

  "boolean" in {

    true.encoded shouldBe List("yes")
    false.encoded shouldBe List("no")

  }

  "string" in {

    "abc".encoded shouldBe List("abc")
    "".encoded shouldBe List("")

  }

  "int" in {

    0.encoded shouldBe List("0")
    1.encoded shouldBe List("1")
    100.encoded shouldBe List("100")
    (-10).encoded shouldBe List("-10")

  }

  "repr" in {

    ("abc" :: 123 :: true :: HNil).encoded shouldBe List("abc", "123", "yes")

  }

  "generic" in {

    case class ClassA(name: String, amount: Int, available: Boolean)

    val instance = ClassA(name = "chair", amount = 12, available = false)
    instance.encoded shouldBe List("chair", "12", "no")

  }

  "coproduct" in {

    sealed trait Shape extends Product with Serializable
    final case class Square(size: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    val shapes = List(Square(3.0), Circle(4.0))
    shapes.head.encoded shouldBe List("3.0")
    shapes.last.encoded shouldBe List("4.0")

  }

}
