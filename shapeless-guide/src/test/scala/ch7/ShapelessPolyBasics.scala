package ch7

import shapeless.{Poly1, Poly2}
import testing.BaseSpec

final class ShapelessPolyBasics extends BaseSpec {

  "Poly1" in {

    object MyPoly extends Poly1 {
      implicit val intCase: Case.Aux[Int, Double] = at(_ / 2.0)
      implicit val stringCase: Case.Aux[String, Int] = at(_.length)
    }

    MyPoly(5) shouldBe 2.5
    MyPoly("abc") shouldBe 3

  }

  "Poly2" in {

    object Multiply extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] = at(_ * _)
      implicit val stringIntCase: Case.Aux[String, Int, String] = at(_ * _)
    }

    Multiply(2, 3) shouldBe 6
    Multiply("abc", 3) shouldBe "abcabcabc"

  }

  "Poly Advanced" in {

    object Total extends Poly1 {
      implicit def base[A: Numeric]: Case.Aux[A, Double] =
        at(Numeric[A].toDouble)
      implicit def option[A: Numeric]: Case.Aux[Option[A], Double] =
        at(opt => opt.fold(0.0)(Numeric[A].toDouble))
      implicit def list[A: Numeric]: Case.Aux[List[A], Double] =
        at(xs => Numeric[A].toDouble(xs.sum))
    }

    Total(1) shouldBe 1.0
    Total(Option(1)) shouldBe 1.0
    Total(List(1, 2, 3)) shouldBe 6.0

  }

}
