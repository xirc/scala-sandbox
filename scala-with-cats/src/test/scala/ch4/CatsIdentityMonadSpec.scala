package ch4

import cats.Id
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import testing.BaseSpec

final class CatsIdentityMonadSpec extends BaseSpec {

  def sumOfSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  }

  sumOfSquare(3: Id[Int], 4: Id[Int]) shouldBe 25

}
