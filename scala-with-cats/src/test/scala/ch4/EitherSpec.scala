package ch4

import cats.syntax.either._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Try

final class EitherSpec extends AnyWordSpecLike with Matchers with EitherValues {

  "right bias" in {

    val either1: Either[String, Int] = Right(1)
    val either2: Either[String, Int] = Right(2)

    val result: Either[String, Int] = for {
      a <- either1
      b <- either2
    } yield a + b
    result shouldBe Right(3)

  }

  "smart constructors" in {

    val a = 1.asRight[String]
    val b = 2.asRight[String]

    val c: Either[String, Int] = for {
      x <- a
      y <- b
    } yield x * x + y * y
    c shouldBe Right(5)

  }

  "smart constructors 2" in {

    def countPositive(nums: List[Int]): Either[String, Int] =
      nums.foldLeft(0.asRight[String]) { (acc, num) =>
        if (num > 0) acc.map(_ + 1)
        else Left("negative")
      }

    countPositive(List(1, 2, 3)) shouldBe Right(3)
    countPositive(List(1, -2, 3)) shouldBe Left("negative")

  }

  "catchOnly" in {

    val e: Either[NumberFormatException, Int] =
      Either.catchOnly[NumberFormatException]("foo".toInt)
    e.left.value shouldBe a[NumberFormatException]
  }

  "catchNonFatal" in {

    val e: Either[Throwable, Nothing] = Either.catchNonFatal(sys.error("bar"))
    e.left.value shouldBe a[RuntimeException]

  }

  "fromTry" in {

    val e: Either[Throwable, Int] = Either.fromTry(Try("foo".toInt))
    e.left.value shouldBe a[NumberFormatException]

  }

  "fromOption" in {

    val e: Either[String, Nothing] = Either.fromOption(None, "bar")
    e.left.value shouldBe "bar"

  }

  "ensure" in {

    val e: Either[String, Int] = (-1).asRight[String].ensure("negative")(_ >= 0)
    e.left.value shouldBe "negative"

  }

  "recover" in {

    val v: Either[String, Int] = "error".asLeft[Int].recover { case _ =>
      -1
    }
    v.value shouldBe -1

  }

  "recoverWith" in {

    val v: Either[String, Int] = "error".asLeft[Int].recoverWith { case _ =>
      Right(-1)
    }
    v.value shouldBe -1

  }

  "leftMap" in {

    val e: Either[String, Int] = "foo".asLeft[Int].leftMap(_.reverse)
    e.left.value shouldBe "oof"

  }

  "bimap" in {

    val r: Either[String, Int] = 6.asRight[String].bimap(_.reverse, _ * 10)
    r.value shouldBe 60

    val l: Either[String, Int] = "bar".asLeft[Int].bimap(_.reverse, _ * 10)
    l.left.value shouldBe "rab"

  }

  "swap" in {

    1.asRight[String] shouldBe Right(1)
    1.asRight[String].swap shouldBe Left(1)

  }

}
