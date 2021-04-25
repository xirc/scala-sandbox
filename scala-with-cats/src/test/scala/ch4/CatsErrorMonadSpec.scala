package ch4

import cats.MonadError
import cats.syntax.all._
import org.scalatest.{EitherValues, TryValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Try

final class CatsErrorMonadSpec
    extends AnyWordSpecLike
    with Matchers
    with TryValues
    with EitherValues {

  def validateAdult[F[_]](
      age: Int
  )(implicit me: MonadError[F, Throwable]): F[Int] = {
    if (age >= 18) {
      age.pure[F]
    } else {
      new IllegalArgumentException("Age must be greater than or equal to 18")
        .raiseError[F, Int]
    }
  }

  validateAdult[Try](18).success.value shouldBe 18
  validateAdult[Try](8).failure.exception shouldBe a[IllegalArgumentException]

  type ExceptionOr[A] = Either[Throwable, A]
  validateAdult[ExceptionOr](-1).left.value shouldBe a[IllegalArgumentException]

}
