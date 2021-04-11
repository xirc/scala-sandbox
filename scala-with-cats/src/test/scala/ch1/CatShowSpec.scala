package ch1

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

object CatShowSpec {
  final case class Cat(name: String, age: Int, color: String)
  object Cat {
    implicit val catShow: Show[Cat] = Show.show { cat =>
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }
  }
}

final class CatShowSpec extends AnyWordSpecLike with Matchers {
  import CatShowSpec._

  "show Cat(???)" in {
    val cat = Cat("Garfield", 41, "ginger and black")
    cat.show shouldBe "Garfield is a 41 year-old ginger and black cat."
  }

}
