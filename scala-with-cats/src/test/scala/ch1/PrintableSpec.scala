package ch1

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

// Type Class
trait Printable[A] {
  def format(value: A): String
}

object Printable {
  // Type Class Instances
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  // interface methods
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(
    p.format(value)
  )
}

final case class Cat(name: String, age: Int, color: String)
object Cat {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age year-old $color cat."
    }
  }
}

final class PrintableSpec extends AnyWordSpecLike with Matchers {

  "format Cat(???)" in {
    val cat = Cat("Garfield", 41, "ginger and black")
    Printable.format(
      cat
    ) shouldBe "Garfield is a 41 year-old ginger and black cat."
  }

  "print Cat(???)" in {
    val cat = Cat("Garfield", 41, "ginger and black")
    Printable.print(cat)
  }

}
