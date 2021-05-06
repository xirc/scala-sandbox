package ch9

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._
import testing.BaseSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class CaseStudy9 extends BaseSpec {

  "foldMap" in {

    def foldMap[A, B: Monoid](xs: Vector[A])(f: A => B): B =
      xs.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

    foldMap(Vector(1, 2, 3))(identity) shouldBe 6

    foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldBe "1! 2! 3! "

    foldMap("Hello world!".toVector)(
      _.toString.toUpperCase
    ) shouldBe "HELLO WORLD!"

  }

  "parallelFoldMap" in {

    def parallelFoldMap[A, B: Monoid](xs: Vector[A])(f: A => B): Future[B] = {
      val NumOfProcessors = Runtime.getRuntime.availableProcessors()
      val BatchSize = (xs.size + NumOfProcessors - 1) / NumOfProcessors
      xs.grouped(BatchSize)
        .toVector
        .traverse(batch => Future(batch.foldMap(f)))
        .map(_.combineAll)
    }

    val xs = Vector.tabulate(1000)(identity)
    parallelFoldMap(xs)(identity).futureValue shouldBe xs.sum

  }

}
