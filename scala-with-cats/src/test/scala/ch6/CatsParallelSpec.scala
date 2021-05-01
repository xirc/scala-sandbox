package ch6

import cats.arrow.FunctionK
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.parallel._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsParallelSpec extends AnyWordSpecLike with Matchers {

  type ErrorOr[A] = Either[Vector[String], A]

  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)

  val error1: ErrorOr[Int] = Left(Vector("error1"))
  val error2: ErrorOr[Int] = Left(Vector("error2"))

  "parTupled" in {

    (error1, error2).tupled shouldBe Left(Vector("error1"))

    (error1, error2).parTupled shouldBe Left(Vector("error1", "error2"))

  }

  "parMapN" in {

    (error1, error2).mapN(_ + _) shouldBe Left(Vector("error1"))

    (error1, error2).parMapN(_ + _) shouldBe Left(Vector("error1", "error2"))

    (success1, success2).parMapN(_ + _) shouldBe Right(3)

  }

  "FunctionK" in {

    object OptionToList extends FunctionK[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa match {
        case None    => List.empty
        case Some(a) => List(a)
      }
    }

    OptionToList(Some(2)) shouldBe List(2)
    OptionToList(None) shouldBe List.empty

  }

  "exercise Parallel List" in {

    (List(1, 2), List(3, 4)).tupled shouldBe
      List((1, 3), (1, 4), (2, 3), (2, 4))

    (List(1, 2), List(3, 4)).parTupled shouldBe
      List((1, 3), (2, 4))

    (List(1, 2, 3), List(4, 5)).parTupled shouldBe
      List((1, 4), (2, 5))

  }

}
