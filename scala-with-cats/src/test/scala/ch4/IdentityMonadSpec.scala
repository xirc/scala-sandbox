package ch4

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class IdentityMonadSpec extends AnyWordSpecLike with Matchers {

  type Id[A] = A
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
    def map[A, B](value: F[A])(f: A => B): F[B]
  }
  val id: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a
    override def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
    override def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)
  }

  id.pure(3) shouldBe 3
  id.flatMap(3)(_ + 2) shouldBe 5
  id.map(3)(_ + 3) shouldBe 6

}
