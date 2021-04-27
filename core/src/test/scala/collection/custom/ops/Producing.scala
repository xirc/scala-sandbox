package collection.custom.ops

import testing.BaseSpec

import scala.collection.Factory
import scala.util.Random

object Producing {

  trait Gen[+A] {
    def gen(): A
  }

  implicit val int: Gen[Int] = new Gen[Int] {
    override def gen(): Int = Random.nextInt()
  }

  implicit val boolean: Gen[Boolean] = new Gen[Boolean] {
    override def gen(): Boolean = Random.nextBoolean()
  }

  implicit val char: Gen[Char] = new Gen[Char] {
    override def gen(): Char = Random.nextPrintableChar()
  }

  implicit def collection[CC[_], A](implicit
      genA: Gen[A],
      factory: Factory[A, CC[A]]
  ): Gen[CC[A]] = new Gen[CC[A]] {
    val GenProbabilityInPercent: Int = 90
    override def gen(): CC[A] = {
      val elements = LazyList.unfold(()) { _ =>
        if (Random.nextInt(100) < 100 - GenProbabilityInPercent) None
        else Some((genA.gen(), ()))
      }
      factory.fromSpecific(elements)
    }
  }

  implicit def string(implicit genC: Gen[Char]): Gen[String] = new Gen[String] {
    val GenProbabilityInPercent = 90
    override def gen(): String = {
      val builder = new StringBuilder()
      while (Random.nextInt(100) < GenProbabilityInPercent) {
        builder.addOne(genC.gen())
      }
      builder.toString()
    }
  }

}

final class Producing extends BaseSpec {
  import Producing._

  collection[Seq, Int].gen() shouldBe a[Seq[_]]
  collection[LazyList, Boolean].gen() shouldBe a[LazyList[_]]
  collection[Set, Int].gen() shouldBe a[Set[_]]
  collection[Array, Boolean].gen() shouldBe a[Array[_]]
  collection[IndexedSeq, Char].gen() shouldBe a[IndexedSeq[_]]
  string.gen() shouldBe a[String]

}
