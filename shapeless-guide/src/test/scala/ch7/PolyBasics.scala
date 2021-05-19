package ch7

import testing.BaseSpec

object PolyBasics {

  trait Case[P, A] {
    type Result
    def apply(a: A): Result
  }

  trait Poly {
    def apply[A](arg: A)(implicit cse: Case[this.type, A]): cse.Result =
      cse.apply(arg)
  }

}

final class PolyBasics extends BaseSpec {
  import PolyBasics._

  object MyPoly extends Poly {

    implicit val intCase: Case[this.type, Int] =
      new Case[this.type, Int] {
        override type Result = Double
        override def apply(a: Int): Double = a / 2.0
      }

    implicit val stringCase: Case[this.type, String] =
      new Case[this.type, String] {
        override type Result = Int
        override def apply(a: String): Int = a.length
      }

  }

  MyPoly(5) shouldBe 2.5
  MyPoly("abc") shouldBe 3

}
