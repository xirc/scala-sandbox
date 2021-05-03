package ch4

import cats.Monad
import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import testing.BaseSpec

import scala.concurrent._

final class CatsMonadSpec extends BaseSpec {

  "option" in {

    val opt1 = Monad[Option].pure(3)
    opt1 shouldBe Option(3)

    val opt2 = Monad[Option].flatMap(opt1)(a => Option(a + 2))
    opt2 shouldBe Option(5)

    val opt3 = Monad[Option].map(opt2)(a => a * 100)
    opt3 shouldBe Option(500)

  }

  "list" in {

    val xs1 = Monad[List].pure(3)
    xs1 shouldBe List(3)

    val xs2 = Monad[List].flatMap(List(1, 2, 3))(x => List(x, x + 10))
    xs2 shouldBe List(1, 11, 2, 12, 3, 13)

    val xs3 = Monad[List].map(xs2)(x => x + 100)
    xs3 shouldBe List(101, 111, 102, 112, 103, 113)

  }

  "future" in {

    import scala.concurrent.ExecutionContext.Implicits._

    val fm = Monad[Future]
    val f = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
    f.futureValue shouldBe 3

  }

  "syntax pure" in {

    import cats.syntax.applicative._

    1.pure[Option] shouldBe Option(1)
    1.pure[List] shouldBe List(1)

  }

  "syntax flatMap & map" in {

    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def sumOfSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x + y * y))

    sumOfSquare(Option(3), Option(2)) shouldBe Option(13)
    sumOfSquare(List(1, 2, 3), List(10, 20)) shouldBe
      List(101, 401, 104, 404, 109, 409)

    def sumOfSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
      for {
        x <- a
        y <- b
      } yield x * x + y * y
    }
    sumOfSquare2(Option(4), Option(3)) shouldBe Option(25)
    sumOfSquare2(List(4, 3), List(2, 1)) shouldBe
      List(20, 17, 13, 10)

  }

}
