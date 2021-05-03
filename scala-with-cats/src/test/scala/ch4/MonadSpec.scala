package ch4

import testing.BaseSpec

import scala.annotation.nowarn

final class MonadSpec extends BaseSpec {

  "try defining map using flatMap and pure" in {

    @nowarn
    trait Monad[F[_]] {

      def pure[A](a: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      def map[A, B](value: F[A])(func: A => B): F[B] =
        flatMap(value)(func andThen pure)

    }

  }

}
