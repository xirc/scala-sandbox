package ch8

import shapeless.ops.coproduct
import shapeless.ops.nat.ToInt
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
  Lazy,
  Nat
}
import testing.BaseSpec

import scala.annotation.nowarn

object CaseStudyRandomValueGenerator {

  // Generates instances of Type `A`
  trait Random[A] {
    def get: A
  }

  object Random {
    def apply[A](implicit random: Random[A]): A =
      random.get
    def instance[A](func: () => A): Random[A] =
      new Random[A] {
        override def get: A = func()
      }
  }

  // Generates random numbers from 0 until 10
  implicit val intRandom: Random[Int] =
    Random.instance[Int](() => scala.util.Random.nextInt(10))

  // Generates random characters from A to Z
  implicit val charRandom: Random[Char] =
    Random.instance[Char](() =>
      ('A'.toInt + scala.util.Random.nextInt(26)).toChar
    )

  // Generates random booleans
  implicit val booleanRandom: Random[Boolean] =
    Random.instance[Boolean](() => scala.util.Random.nextBoolean())

  implicit def hnilRandom: Random[HNil] =
    Random.instance[HNil](() => HNil)

  implicit def hlistRandom[H, T <: HList](implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T]
  ): Random[H :: T] =
    Random.instance { () =>
      hRandom.value.get :: tRandom.get
    }

  implicit def genericRandom[A, R](implicit
      generic: Generic.Aux[A, R],
      random: Random[R]
  ): Random[A] =
    Random.instance { () =>
      generic.from(random.get)
    }

  implicit def cnilRandom: Random[CNil] =
    Random.instance(() => throw new Exception())

  implicit def coproductRandom[H, T <: Coproduct, L <: Nat](implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T],
      @nowarn tLength: coproduct.Length.Aux[T, L],
      tLengthAsInt: ToInt[L]
  ): Random[H :+: T] =
    Random.instance { () =>
      val length = 1 + tLengthAsInt()
      val chooseH = scala.util.Random.nextDouble() < (1.0 / length)
      if (chooseH)
        Inl(hRandom.value.get)
      else
        Inr(tRandom.get)
    }

}

@nowarn("cat=lint-byname-implicit")
final class CaseStudyRandomValueGenerator extends BaseSpec {
  import CaseStudyRandomValueGenerator._

  "Random[Int]" in {

    for (_ <- 1 to 1000) {
      (0 until 10).contains(Random[Int]) shouldBe true
    }

  }

  "Random[Char]" in {

    for (_ <- 1 to 1000) {
      ('A' to 'Z').contains(Random[Char]) shouldBe true
    }

  }

  "Random[Boolean]" in {

    Set(true, false).contains(Random[Boolean]) shouldBe true

  }

  "Random[HNil]" in {

    Random[HNil] shouldBe HNil

  }

  "Random[HList]" in {

    val int :: char :: boolean :: HNil = Random[Int :: Char :: Boolean :: HNil]
    (0 until 10).contains(int) shouldBe true
    ('A' to 'Z').contains(char) shouldBe true
    Set(true, false).contains(boolean) shouldBe true

  }

  "Random[<products>]" in {

    case class ClassA(char: Char, int: Int, boolean: Boolean)
    val instance = Random[ClassA]
    ('A' to 'Z').contains(instance.char) shouldBe true
    (0 until 10).contains(instance.int) shouldBe true
    Set(true, false).contains(instance.boolean) shouldBe true

  }

  "Random[<coproducts>]" in {

    sealed trait Color extends Product with Serializable
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
    val colors = Set(Red, Green, Blue)

    for (_ <- 0 until 100) {
      colors.contains(Random[Color]) shouldBe true
    }

  }

}
