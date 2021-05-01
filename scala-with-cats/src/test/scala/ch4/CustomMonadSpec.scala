package ch4

import cats.Monad
import cats.syntax.all._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CustomMonadSpec extends AnyWordSpecLike with Matchers {

  "exercise" in {

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)

    implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
      override def pure[A](x: A): Tree[A] =
        Leaf(x)
      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
        fa match {
          case Branch(left, right) =>
            Branch(flatMap(left)(f), flatMap(right)(f))
          case Leaf(value) =>
            f(value)
        }
      override def tailRecM[A, B](
          a: A
      )(f: A => Tree[Either[A, B]]): Tree[B] = {
        flatMap(f(a)) {
          case Left(value) =>
            tailRecM(value)(f)
          case Right(value) =>
            Leaf(value)
        }
      }
    }

    val tree1 =
      branch(leaf(100), leaf(200))
        .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
    tree1 shouldBe branch(
      branch(leaf(99), leaf(101)),
      branch(leaf(199), leaf(201))
    )

    val tree3 = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c
    tree3 shouldBe branch(
      branch(
        branch(leaf(89), leaf(91)),
        branch(leaf(109), leaf(111))
      ),
      branch(
        branch(leaf(189), leaf(191)),
        branch(leaf(209), leaf(211))
      )
    )

  }

}
