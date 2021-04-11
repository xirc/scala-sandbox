package ch1

import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

object CatEqSpec {

  final case class Cat(name: String, age: Int, color: String)
  object Cat {
    implicit val catEq: Eq[Cat] = Eq.instance { (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }
  }

}

final class CatEqSpec extends AnyWordSpecLike with Matchers {
  import CatEqSpec._

  private val cat1 = Cat("Garfield", 38, "orange and black")
  private val cat2 = Cat("Heathcliff", 33, "orange and black")

  "compare equality & inequality" in {

    (cat1 eqv cat2) shouldBe false
    (cat1 =!= cat2) shouldBe true

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    (optionCat1 eqv optionCat2) shouldBe false
    (optionCat1 =!= optionCat2) shouldBe true

  }

}
