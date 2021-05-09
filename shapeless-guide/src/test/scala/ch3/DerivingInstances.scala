package ch3

import shapeless.{
  :+:,
  ::,
  CNil,
  Coproduct,
  Generic,
  HList,
  HNil,
  Inl,
  Inr,
  Lazy
}
import testing.BaseSpec

import scala.annotation.nowarn

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
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] =
      instance { case h :: t =>
        hEncoder.value.encode(h) ::: tEncoder.encode(t)
      }

    implicit def genericEncoder[A, R](implicit
        gen: Generic.Aux[A, R],
        enc: Lazy[CsvEncoder[R]]
    ): CsvEncoder[A] =
      instance(a => enc.value.encode(gen.to(a)))

    implicit val cnilEncoder: CsvEncoder[CNil] =
      instance(_ => throw new Exception())

    implicit def coproductEncoder[H, T <: Coproduct](implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] =
      instance {
        case Inl(h) => hEncoder.value.encode(h)
        case Inr(t) => tEncoder.encode(t)
      }

  }

}

final class DerivingInstances extends BaseSpec {
  import DerivingInstances._

  "boolean" in {

    val encoder = CsvEncoder[Boolean]
    encoder.encode(true) shouldBe List("yes")
    encoder.encode(false) shouldBe List("no")

  }

  "string" in {

    val encoder = CsvEncoder[String]
    encoder.encode("abc") shouldBe List("abc")
    encoder.encode("") shouldBe List("")

  }

  "int" in {

    val encoder = CsvEncoder[Int]
    encoder.encode(0) shouldBe List("0")
    encoder.encode(1) shouldBe List("1")
    encoder.encode(100) shouldBe List("100")
    encoder.encode(-10) shouldBe List("-10")

  }

  "repr" in {

    @nowarn("cat=lint-byname-implicit")
    val encoder = CsvEncoder[String :: Int :: Boolean :: HNil]
    val instance = "abc" :: 123 :: true :: HNil
    encoder.encode(instance) shouldBe List("abc", "123", "yes")

  }

  "generic" in {

    case class ClassA(name: String, amount: Int, available: Boolean)

    @nowarn("cat=lint-byname-implicit")
    val encoder = CsvEncoder[ClassA]
    val instance = ClassA(name = "chair", amount = 12, available = false)
    encoder.encode(instance) shouldBe List("chair", "12", "no")

  }

  "coproduct" in {

    sealed trait Shape extends Product with Serializable
    final case class Square(size: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    @nowarn("cat=lint-byname-implicit")
    val encoder = CsvEncoder[Shape]
    val shapes = List(Square(3.0), Circle(4.0))
    encoder.encode(shapes.head) shouldBe List("3.0")
    encoder.encode(shapes.last) shouldBe List("4.0")

  }

  "recursive" in {

    sealed trait Tree[A] extends Product with Serializable
    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)
      def leaf[A](value: A): Tree[A] =
        Leaf(value)
    }
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    @nowarn("cat=lint-byname-implicit")
    val encoder = CsvEncoder[Tree[Int]]
    val tree: Tree[Int] = Tree.branch(Tree.leaf(1), Tree.leaf(2))
    encoder.encode(tree) shouldBe List("1", "2")

  }

}
