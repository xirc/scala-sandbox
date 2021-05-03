package ch3

import testing.BaseSpec

final class ContravariantFunctorSpec extends BaseSpec {

  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](f: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String =
        self.format(f(value))
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s"'$value'"
  }
  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if (value) "yes" else "no"
  }

  "printable" in {
    format("hello") shouldBe "'hello'"
    format(true) shouldBe "yes"
  }

  "contramap" in {

    final case class Box[A](value: A)
    implicit def stringBoxPrintable[A](implicit
        ev: Printable[A]
    ): Printable[Box[A]] =
      ev.contramap(box => box.value)

    format(Box("hello world")) shouldBe "'hello world'"
    format(Box(true)) shouldBe "yes"

  }

}
