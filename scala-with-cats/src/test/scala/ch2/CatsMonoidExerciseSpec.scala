package ch2

import cats.Monoid
import cats.syntax.semigroup._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

object CatsMonoidExerciseSpec {

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }

}

final class CatsMonoidExerciseSpec extends AnyWordSpecLike with Matchers {
  import CatsMonoidExerciseSpec._

  "sum a list of integers" in {

    add(List(1, 2, 3)) shouldBe 6
    add(List.empty[Int]) shouldBe 0

  }

  "sum a list of optional integers" in {

    add(List(Option(1), Option(2), None, Option(4))) shouldBe Option(7)

  }

  "sum a list of orders" in {
    case class Order(totalCost: Double, quantity: Double)
    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0, 0)
      override def combine(x: Order, y: Order): Order = {
        val totalCost = x.totalCost + y.totalCost
        val quantity = x.quantity + y.quantity
        Order(totalCost, quantity)
      }
    }

    add(List(Order(12, 4), Order(7, 9), Order(8, 10))) shouldBe Order(27, 23)

  }

}
