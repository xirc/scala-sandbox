package ch7

import cats.Applicative
import cats.instances.future._
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

}
