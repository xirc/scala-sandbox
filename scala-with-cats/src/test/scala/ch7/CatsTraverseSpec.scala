package ch7

import cats.Traverse
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import testing.BaseSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class CatsTraverseSpec extends BaseSpec {

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  val expectedUpTimes = List(1020, 960, 840)

  def getUpTime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val times: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUpTime)
  times.futureValue shouldBe expectedUpTimes

  hostnames.traverse(getUpTime).futureValue shouldBe expectedUpTimes

  val numbers = List(Future(1), Future(2), Future(3))
  Traverse[List].sequence(numbers).futureValue shouldBe List(1, 2, 3)

  numbers.sequence.futureValue shouldBe List(1, 2, 3)

}
