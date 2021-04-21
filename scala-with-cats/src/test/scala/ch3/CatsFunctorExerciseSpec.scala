package ch3

import cats.Functor
import cats.syntax.functor._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsFunctorExerciseSpec extends AnyWordSpecLike with Matchers {

  "Branching out with Functors" in {

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]
    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)
      def leaf[A](value: A): Tree[A] =
        Leaf(value)
    }

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
        fa match {
          case Branch(left, right) =>
            Branch(map(left)(f), map(right)(f))
          case Leaf(value) =>
            Leaf(f(value))
        }
      }
    }

    val tree = Tree.branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)
    )
    val mappedTree = tree
      .map(_ * 2)
      .map(x => s"$x!")
    val expectedTree: Tree[String] = Branch(
      Branch(Leaf("2!"), Leaf("4!")),
      Leaf("6!")
    )
    mappedTree shouldBe expectedTree

  }

}
