package ch7

import cats.Applicative
import cats.data.Validated
import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class TraverseSpec
    extends AnyWordSpecLike
    with Matchers
    with ScalaFutures {

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  val expectedUpTimes = List(1020, 960, 840)

  def getUpTime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  def listTraverse[F[_]: Applicative, A, B](xs: List[A])(
      f: A => F[B]
  ): F[List[B]] =
    xs.foldLeft(List.empty[B].pure[F]) { (acc, x) =>
      (acc, f(x)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, A](xs: List[F[A]]): F[List[A]] =
    listTraverse(xs)(identity)

  "foldLeft" in {

    val times = hostnames.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
      for {
        acc <- acc
        uptime <- getUpTime(hostname)
      } yield acc :+ uptime
    }

    times.futureValue shouldBe expectedUpTimes

  }

  "Future|traverse" in {

    val times = Future.traverse(hostnames)(getUpTime)
    times.futureValue shouldBe expectedUpTimes

  }

  "Future|sequence" in {

    val times = Future.sequence(hostnames.map(getUpTime))
    times.futureValue shouldBe expectedUpTimes

  }

  "using Applicative" in {

    val times = hostnames.foldLeft(List.empty[Int].pure[Future]) {
      (acc, hostname) =>
        (acc, getUpTime(hostname)).mapN(_ :+ _)
    }
    times.futureValue shouldBe expectedUpTimes

    listTraverse(hostnames)(getUpTime).futureValue shouldBe expectedUpTimes

  }

  "Traversing with Vectors" in {

    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe
      Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe
      Vector(
        List(1, 3, 5),
        List(1, 3, 6),
        List(1, 4, 5),
        List(1, 4, 6),
        List(2, 3, 5),
        List(2, 3, 6),
        List(2, 4, 5),
        List(2, 4, 6)
      )

  }

  "Traversing with Options" in {

    def process(inputs: List[Int]): Option[List[Int]] =
      listTraverse(inputs)(n => if (n % 2 == 0) Option(n) else None)

    process(List(2, 4, 6)) shouldBe Option(List(2, 4, 6))
    process(List(1, 2, 3)) shouldBe None

  }

  "Traversing with Validated" in {

    type ErrorsOr[A] = Validated[List[String], A]

    def process(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if (n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }

    process(List(2, 4, 6)) shouldBe
      Validated.valid(List(2, 4, 6))

    process(List(1, 2, 3)) shouldBe
      Validated.invalid(List("1 is not even", "3 is not even"))

  }

}
