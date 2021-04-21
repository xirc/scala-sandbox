package ch3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class InvariantFunctorSpec extends AnyWordSpecLike with Matchers {

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit ev: Codec[A]): String =
    ev.encode(value)

  def decode[A](value: String)(implicit ev: Codec[A]): A =
    ev.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  "stringCodec" in {

    encode("hello") shouldBe "hello"
    decode[String]("world") shouldBe "world"

  }

  "imap" in {

    encode(1) shouldBe "1"
    decode[Int]("1") shouldBe 1

    encode(true) shouldBe "true"
    decode[Boolean]("true") shouldBe true

  }

  "imap exercise" in {

    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap(_.toDouble, _.toString)

    encode(123.4) shouldBe "123.4"
    decode[Double]("123.4") shouldBe 123.4

    final case class Box[A](value: A)
    implicit def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] =
      ev.imap(value => Box(value), box => box.value)

    encode(Box(123.4)) shouldBe "123.4"
    decode[Box[Double]]("123.4") shouldBe Box(123.4)

  }

}
